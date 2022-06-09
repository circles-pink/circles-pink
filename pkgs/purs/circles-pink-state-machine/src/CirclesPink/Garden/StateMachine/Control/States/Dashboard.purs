module CirclesPink.Garden.StateMachine.Control.States.Dashboard
  ( dashboard
  , fetchUsersBinarySearch
  ) where

import Prelude

import CirclesCore (TrustNode, User)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', dropError, retryUntil, subscribeRemoteReport)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.StateMachine.State.Dashboard (Trust, TrustState, initTrusted, initUntrusted)
import Control.Monad.Except (catchError, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Convertable (convert)
import Data.Array (catMaybes, drop, find, take)
import Data.Array as A
import Data.Either (Either(..), hush, isRight)
import Data.Int (floor, toNumber)
import Data.Map (Map, fromFoldable, lookup, update)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (default, onMatch)
import Debug.Extra (todo)
import Foreign.Object (insert)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import RemoteData (RemoteData, _failure, _loading, _success)
import Type.Row (type (+))
import Undefined (undefined)
import Wallet.PrivateKey (PrivateKey, unsafeAddrFromString)
import Wallet.PrivateKey as P

type ErrFetchUsersBinarySearch r = (err :: Unit | r)

splitArray :: forall a. Array a -> Array a /\ Array a
splitArray xs =
  let
    count = floor ((toNumber $ A.length xs) / 2.0)
  in
    take count xs /\ drop count xs

fetchUsersBinarySearch :: forall r m. Monad m => Env.Env m -> PrivateKey -> Array W3.Address -> ExceptV (ErrFetchUsersBinarySearch + r) m (Array (Either W3.Address User))
fetchUsersBinarySearch _ _ xs
  | A.length xs == 0 = pure []

fetchUsersBinarySearch env privKey xs
  | A.length xs == 1 = do
      eitherUsers <- lift $ runExceptT $ env.getUsers privKey [] (map convert xs)
      case eitherUsers of
        Left _ -> pure $ map Left xs
        Right ok -> pure $ map Right ok

fetchUsersBinarySearch env privKey xs = do
  eitherUsers <- lift $ runExceptT $ env.getUsers privKey [] (map convert xs)
  case eitherUsers of
    Left _ ->
      let
        (ls /\ rs) = splitArray xs
      in
        fetchUsersBinarySearch env privKey ls <> fetchUsersBinarySearch env privKey rs
    Right ok -> pure $ map Right ok

dashboard
  :: forall m
   . Monad m
  => Env.Env m
  -> { logout :: ActionHandler' m Unit S.DashboardState ("landing" :: S.LandingState)
     , getTrusts :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     , addTrustConnection :: ActionHandler' m String S.DashboardState ("dashboard" :: S.DashboardState)
     , removeTrustConnection :: ActionHandler' m String S.DashboardState ("dashboard" :: S.DashboardState)
     , getBalance :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     , transfer ::
         ActionHandler' m
           { from :: String
           , to :: String
           , value :: String
           , paymentNote :: String
           }
           S.DashboardState
           ("dashboard" :: S.DashboardState)
     , getUsers ::
         ActionHandler' m
           { userNames :: Array String
           , addresses :: Array P.Address
           }
           S.DashboardState
           ("dashboard" :: S.DashboardState)
     , userSearch ::
         ActionHandler' m
           { query :: String
           }
           S.DashboardState
           ("dashboard" :: S.DashboardState)
     , redeploySafeAndToken :: ActionHandler' m Unit S.DashboardState ("dashboard" :: S.DashboardState)
     }
dashboard env =
  { logout: \_ _ _ -> pure unit
  , getTrusts
  , addTrustConnection
  , removeTrustConnection
  , getBalance
  , getUsers
  , transfer
  , userSearch
  , redeploySafeAndToken
  }
  where
  getTrusts set st _ =
    void do
      runExceptT do
        _ <- syncTrusts set st
          # retryUntil env (const { delay: 1000 }) (\r _ -> isRight r) 0

        _ <- syncTrusts set st
          # retryUntil env (const { delay: 10000 }) (\_ _ -> false) 0

        pure unit

  syncTrusts set st i =
    do
      let
        mapTrust :: Array User -> TrustNode -> W3.Address /\ Trust
        mapTrust foundUsers tn = convert tn.safeAddress /\
          { isOutgoing: tn.isOutgoing
          , user: find (\u -> u.safeAddress == tn.safeAddress) foundUsers
          , trustState: if tn.isIncoming then initTrusted else initUntrusted
          }

      trustNodes <-
        env.trustGetNetwork st.privKey
          # (\x -> subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r }) x i)
          # dropError
      users <-
        fetchUsersBinarySearch env st.privKey (map (convert <<< _.safeAddress) trustNodes)
          # dropError
      let
        foundUsers = catMaybes $ map hush users
      lift $ set \st' -> S._dashboard st'
        { trusts = trustNodes <#> mapTrust foundUsers # M.fromFoldable }

  getUsers set st { userNames, addresses } = do
    set \st' -> S._dashboard st' { getUsersResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV (S.ErrGetUsers + ()) _ _
      task = env.getUsers st.privKey userNames addresses
    result <- runExceptT $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { getUsersResult = _failure e }
      Right u -> set \st' -> S._dashboard st' { getUsersResult = _success u }

  addTrustConnection set st u =
    void do
      runExceptT do
        let addr = unsafePartial $ P.unsafeAddrFromString u
        -- lift $ set \st' ->  S._dashboard st' { trusts = update (\t -> pure $ t { trustState = todo :: TrustState }) (convert addr) st'.trusts }
        _ <-
          env.addTrustConnection st.privKey addr st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustAddResult = insert u r st.trustAddResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # dropError
        -- lift $ set \st' -> S._dashboard st'
        --   { trusts = st'.trusts
        --       # update (\t -> pure $ t { trustState = initTrusted }) (convert addr)
        --   }
        -- _ <- syncTrusts set st
        --   # retryUntil env (const { delay: 1500 }) (\_ n -> n == 10) 0
        --   # dropError
        pure unit

  redeploySafeAndToken _ st _ = void do
    runExceptT
      do
        -- First deploy always fails
        _ <- env.deploySafe st.privKey `catchError` \_ -> pure unit
        _ <- (env.deployToken st.privKey <#> const unit) `catchError` \_ -> pure unit
        -- Second deploy works
        _ <- env.deploySafe st.privKey
        _ <- (env.deployToken st.privKey <#> const unit)
        pure unit

  removeTrustConnection set st u =
    void do
      runExceptT do
        let addr = unsafePartial $ P.unsafeAddrFromString u
        -- lift $ set \st' -> S._dashboard st' { trusts = update (\t -> pure $ t { trustState = todo :: TrustState }) (convert addr) st'.trusts }
        _ <-
          env.removeTrustConnection st.privKey addr st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustRemoveResult = insert u r st.trustRemoveResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # dropError
        lift $ set \st' -> S._dashboard st'
          { trusts = st'.trusts
              # update (\t -> pure $ t { trustState = initUntrusted }) (convert addr)
          }
        -- _ <- syncTrusts set st
        --   # retryUntil env (const { delay: 1500 }) (\_ n -> n == 10) 0
        --   # dropError

        pure unit

  getBalance set st _ =
    void do
      runExceptT do
        _ <-
          env.getBalance st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 2000 }) (\_ n -> n == 3) 0

        checkPayout <-
          env.checkUBIPayout st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { checkUBIPayoutResult = r })
            # retryUntil env (const { delay: 5000 }) (\r n -> n == 5 || isRight r) 0

        when ((length $ CC.bnToStr checkPayout) >= 18) do
          env.requestUBIPayout st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { requestUBIPayoutResult = r })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 5 || isRight r) 0
            # void
        _ <-
          env.getBalance st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 2000 }) (\r n -> n == 5 || isRight r) 0

        _ <-
          env.getBalance st.privKey st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 10000 }) (\_ _ -> false) 0

        pure unit

  userSearch set st options = do
    void do
      runExceptT do
        env.userSearch st.privKey options
          # (\x -> subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { userSearchResult = r }) x 0)

  transfer set st { from, to, value, paymentNote } = do
    set \st' -> S._dashboard st' { transferResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV (S.ErrTokenTransfer + ()) _ _
      task = env.transfer st.privKey (unsafePartial $ unsafeAddrFromString from) (unsafePartial $ unsafeAddrFromString to) value paymentNote
    result <- runExceptT $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { transferResult = _failure e }
      Right h -> set \st' -> S._dashboard st' { transferResult = _success h }

