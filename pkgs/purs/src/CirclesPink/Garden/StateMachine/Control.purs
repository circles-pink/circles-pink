module CirclesPink.Garden.StateMachine.Control
  ( circlesControl
  ) where

import Prelude
import CirclesPink.Garden.StateMachine (_circlesStateMachine)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Control.Env as E
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans, lift, runExceptT)
import Data.Either (Either(..))
import Data.Variant (Variant, default, onMatch)
import RemoteData (RemoteData, _failure, _loading, _success)
import Stadium.Control as C
import Type.Row (type (+))
import Wallet.PrivateKey as P

circlesControl ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  E.Env m -> ((S.CirclesState -> S.CirclesState) -> t m Unit) -> S.CirclesState -> CirclesAction -> t m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { landing:
        { signUp: \set _ _ -> set $ \_ -> S.init
        , signIn: \set _ _ -> set $ \_ -> S.initLogin
        }
    , infoGeneral:
        { next:
            \set _ _ -> set $ \st -> S._askUsername st { direction = D._forwards }
        }
    , askUsername:
        { prev:
            \set _ _ -> set $ \st -> S._infoGeneral st { direction = D._backwards }
        , setUsername:
            \set _ username -> do
              set $ \st -> S._askUsername st { username = username }
              set
                $ \st ->
                    S._askUsername st { usernameApiResult = _loading :: RemoteData CirclesError { isValid :: Boolean } }
              result <- lift $ runExceptT $ env.apiCheckUserName username
              set
                $ \st ->
                    if username == st.username then case result of
                      Left e -> S._askUsername st { usernameApiResult = _failure e }
                      Right x -> S._askUsername st { usernameApiResult = _success x }
                    else
                      S._askUsername st
        , next:
            \set _ _ ->
              set
                $ \st ->
                    let
                      usernameValid =
                        default false
                          # onMatch
                              { success: (\r -> r.isValid) }
                    in
                      if usernameValid st.usernameApiResult then
                        S._askEmail st { direction = D._forwards }
                      else
                        S._askUsername st { direction = D._forwards }
        }
    , askEmail:
        { prev:
            \set _ _ -> set $ \st -> S._askUsername st { direction = D._backwards }
        , setEmail:
            \set _ email -> do
              set $ \st -> S._askEmail st { email = email }
              set
                $ \st ->
                    S._askEmail st { emailApiResult = _loading :: RemoteData CirclesError { isValid :: Boolean } }
              result <- lift $ runExceptT $ env.apiCheckEmail email
              set
                $ \st ->
                    if email == st.email then case result of
                      Left e -> S._askEmail st { emailApiResult = _failure e }
                      Right x -> S._askEmail st { emailApiResult = _success x }
                    else
                      S._askEmail st
              pure unit
        , setTerms: \set _ _ -> set $ \st -> S._askEmail st { terms = not st.terms }
        , setPrivacy: \set _ _ -> set $ \st -> S._askEmail st { privacy = not st.privacy }
        , next: askEmailNext
        }
    , infoSecurity:
        { prev:
            \set _ _ -> set $ \st -> S._askEmail st { direction = D._backwards }
        , next:
            \set _ _ -> do
              pk <- lift $ env.generatePrivateKey
              set
                $ \st ->
                    if P.zeroKey == st.privateKey then
                      S._magicWords st { privateKey = pk, direction = D._forwards }
                    else
                      S._magicWords st { direction = D._forwards }
        }
    , magicWords:
        { prev:
            \set _ _ -> set $ \st -> S._infoSecurity st { direction = D._backwards }
        , newPrivKey:
            \set _ _ -> do
              pk <- lift $ env.generatePrivateKey
              set $ \st -> S._magicWords st { privateKey = pk }
        , next: \set _ _ -> set $ \st -> S._submit st { direction = D._forwards }
        }
    , submit:
        { prev: \set _ _ -> set $ \st -> S._magicWords st { direction = D._backwards }
        , submit:
            \_ st _ -> do
              let
                address = P.privKeyToAddress st.privateKey

                nonce = P.addressToNonce address
              result :: Either (Variant (E.GetSafeAddressError + E.RegisterError + ())) Unit <-
                (lift <<< runExceptT) do
                  safeAddress <- env.getSafeAddress st.privateKey
                  _ <- env.safePrepareDeploy st.privateKey
                  env.userRegister
                    st.privateKey
                    { email: st.email
                    , nonce
                    , safeAddress
                    , username: st.username
                    }
              pure unit
        }
    , dashboard:
        { logout:
            \set _ _ -> pure unit
        }
    , login:
        { login: loginLogin
        , signUp:
            \set _ _ -> set $ \_ -> S.init
        , setMagicWords:
            \set _ words -> set $ \st -> S._login st { magicWords = words }
        }
    , trusts:
        { continue:
            \set _ _ -> pure unit
        }
    }
  where
  askEmailNext :: ActionHandler t m Unit S.UserData ( "askEmail" :: S.UserData, "infoSecurity" :: S.UserData )
  askEmailNext set _ _ =
    set
      $ \st ->
          let
            emailValid =
              default false
                # onMatch
                    { success: (\r -> r.isValid) }
          in
            if (emailValid st.emailApiResult) && st.terms && st.privacy then
              S._infoSecurity st { direction = D._forwards }
            else
              S._askEmail st { direction = D._forwards }

  loginLogin :: ActionHandler t m Unit S.LoginState ( "login" :: S.LoginState, "trusts" :: S.TrustState, "dashboard" :: S.DashboardState )
  loginLogin set st _ = do
    let
      mnemonic = P.getMnemonicFromString st.magicWords

      privKey = P.mnemonicToKey mnemonic
    eitherUser <- (lift <<< runExceptT) $ env.userResolve privKey
    case eitherUser of
      Left e -> set $ \st' -> S._login st' { error = pure e }
      Right u -> set $ \_ -> S._trusts { user: u, trusts: [], privKey }

type ActionHandler :: forall k. (k -> Type -> Type) -> k -> Type -> Type -> Row Type -> Type
type ActionHandler t m a s v
  = ((s -> Variant v) -> t m Unit) -> s -> a -> t m Unit
