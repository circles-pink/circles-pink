module CirclesPink.Garden.StateMachine.Control
  ( Env
  , GetSafeAddressError
  , PrepareSafeDeployError
  , RegisterError
  , circlesControl
  ) where

import Prelude
import CirclesPink.Garden.CirclesCore (UserOptions)
import CirclesPink.Garden.StateMachine (_circlesStateMachine)
import CirclesPink.Garden.StateMachine.Action (CirclesAction)
import CirclesPink.Garden.StateMachine.Direction as D
import CirclesPink.Garden.StateMachine.Error (CirclesError)
import CirclesPink.Garden.StateMachine.State (CirclesState, User)
import CirclesPink.Garden.StateMachine.State as S
import Control.Monad.Except (class MonadTrans, ExceptT, lift, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Typelevel.Undefined (undefined)
import Data.Variant (Variant, case_, default, on, onMatch)
import Debug (spy)
import Effect.Class.Console (logShow)
import Effect.Exception (Error)
import RemoteData (RemoteData, _failure, _loading, _success)
import Stadium.Control as C
import Type.Row (type (+))
import Wallet.PrivateKey (Address, Nonce, PrivateKey)
import Wallet.PrivateKey as P

type RegisterError r
  = ( errService :: Unit, errNative :: Error | r )

type GetSafeAddressError r
  = ( errNative :: Error | r )

type PrepareSafeDeployError r
  = ( errNative :: Error | r )

type Env m
  = { apiCheckUserName :: String -> ExceptT CirclesError m { isValid :: Boolean }
    , apiCheckEmail :: String -> ExceptT CirclesError m { isValid :: Boolean }
    , generatePrivateKey :: m PrivateKey
    , userRegister :: forall r. PrivateKey -> UserOptions -> ExceptV (RegisterError + r) m Unit
    , getSafeAddress :: forall r. { nonce :: Nonce, privKey :: PrivateKey } -> ExceptV (GetSafeAddressError + r) m Address
    , safePrepareDeploy :: forall r. { nonce :: Nonce, privKey :: PrivateKey } -> ExceptV (PrepareSafeDeployError + r) m Address
    , requestUserData :: Address -> ExceptT CirclesError m User
    }

-- {"id":1,"username":"pink3","safeAddress":"0xE853AF934C6c05D4262F6F868C304036014b040C","avatarUrl":null}
circlesControl ::
  forall t m.
  Monad m =>
  MonadTrans t =>
  Monad (t m) =>
  Env m -> ((CirclesState -> CirclesState) -> t m Unit) -> CirclesState -> CirclesAction -> t m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { landing:
        { signUp:
            \set _ _ -> set $ \_ -> S.init
        , signIn:
            \set _ _ -> set $ \_ -> S.initLogin
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
        , next:
            \set _ _ ->
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
              result :: Either (Variant (GetSafeAddressError + RegisterError + ())) Unit <-
                (lift <<< runExceptT) do
                  safeAddress <- env.getSafeAddress { nonce, privKey: st.privateKey }
                  let
                    x = spy "safeAddress" safeAddress
                  _ <- env.safePrepareDeploy { nonce, privKey: st.privateKey }
                  let
                    y = spy "safePrepareDeploy" ""
                  env.userRegister
                    st.privateKey
                    { email: st.email
                    , nonce
                    , safeAddress
                    , username: st.username
                    }
              let
                x = spy "result" result
              pure unit
        }
    , dashboard:
        { logout:
            \set _ _ -> pure unit
        }
    , login:
        { login:
            \set st _ -> do
              let
                mnemonic = P.getMnemonicFromString st.magicWords

                privKey = P.mnemonicToKey mnemonic

                address = P.privKeyToAddress privKey

                nonce = P.addressToNonce address
              safeAddress <- lift $ runExceptT $ env.getSafeAddress { nonce, privKey }
              pure unit
        , back:
            \set _ _ -> pure unit
        , setMagicWords:
            \set _ words -> set $ \st -> S._login st { magicWords = words }
        }
    , trusts:
        { continue:
            \set _ _ -> pure unit
        }
    }
