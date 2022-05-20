module CirclesPink.Garden.StateMachine.Control.States.Dashboard
  ( dashboard
  , fetchUsersBinarySearch
  ) where

import Prelude

import CirclesCore (User, TrustNode)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler', runExceptT')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.StateMachine.State.Dashboard (Trust)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Convertable (convert)
import Data.Array (catMaybes, drop, find, take)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush, isRight)
import Data.Int (floor, toNumber)
import Data.Map as M
import Data.String (length)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Foreign.Object (insert)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import RemoteData (RemoteData, _failure, _loading, _success)
import RemoteReport (RemoteReport)
import Type.Row (type (+))
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
  }
  where
  getTrusts set st _ =
    let
      mapTrust :: Array User -> TrustNode -> W3.Address /\ Trust
      mapTrust foundUsers t = convert t.safeAddress /\ user
        where
        user =
          { isLoading: false
          , isIncoming: t.isIncoming
          , isOutgoing: t.isOutgoing
          , user: find (\u -> u.safeAddress == t.safeAddress) foundUsers
          }
    in
      void do
        runExceptT do
          trusts <-
            runExceptT (env.trustGetNetwork st.privKey)
              # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r })
              # retryUntil env (const { delay: 5000 }) (\r _ -> isRight r) 0
              <#> lmap (const unit)
              # ExceptT
          users <-
            runExceptT (fetchUsersBinarySearch env st.privKey (map (convert <<< _.safeAddress) trusts))
              <#> lmap (const unit)
              # ExceptT
          let
            foundUsers = catMaybes $ map hush users
          lift $ set \st' -> S._dashboard st' { trusts = trusts <#> mapTrust foundUsers # M.fromFoldable }
          _ <-
            runExceptT (env.trustGetNetwork st.privKey)
              # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r })
              # retryUntil env (const { delay: 15000 }) (\_ _ -> false) 0
              <#> lmap (const unit)
              # ExceptT
          pure unit

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
        _ <-
          runExceptT (env.addTrustConnection st.privKey (unsafePartial $ P.unsafeAddrFromString u) st.user.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustAddResult = insert u r st.trustAddResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # ExceptT
        _ <-
          runExceptT (env.trustGetNetwork st.privKey)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r })
            # retryUntil env (const { delay: 1000 }) (\_ n -> n == 10) 0
            # ExceptT
        pure unit

  removeTrustConnection set st u =
    void do
      runExceptT do
        _ <-
          runExceptT (env.removeTrustConnection st.privKey (unsafePartial $ P.unsafeAddrFromString u) st.user.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustRemoveResult = insert u r st.trustRemoveResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # ExceptT
        _ <-
          runExceptT (env.trustGetNetwork st.privKey)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r })
            # retryUntil env (const { delay: 1000 }) (\_ n -> n == 10) 0
            # ExceptT
        pure unit

  getBalance set st _ =
    void do
      runExceptT do
        _ <-
          runExceptT (env.getBalance st.privKey st.user.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 2000 }) (\_ n -> n == 3) 0
            # ExceptT
        checkPayout <-
          runExceptT (env.checkUBIPayout st.privKey st.user.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { checkUBIPayoutResult = r })
            # retryUntil env (const { delay: 5000 }) (\r n -> n == 5 || isRight r) 0
            # ExceptT
        let
          payoutAmount = CC.bnToStr checkPayout
        when ((length payoutAmount) >= 18) do
          runExceptT (env.requestUBIPayout st.privKey st.user.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { requestUBIPayoutResult = r })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 5 || isRight r) 0
            # ExceptT
            # void
        _ <-
          runExceptT (env.getBalance st.privKey st.user.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 2000 }) (\r n -> n == 5 || isRight r) 0
            # ExceptT
        _ <-
          runExceptT (env.getBalance st.privKey st.user.safeAddress)
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { getBalanceResult = r })
            # retryUntil env (const { delay: 15000 }) (\_ _ -> false) 0
            # ExceptT
        pure unit

  userSearch set st options = do
    set \st' -> S._dashboard st' { userSearchResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV (S.ErrUserSearch ()) _ _
      task = env.userSearch st.privKey options
    result <- runExceptT' $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { userSearchResult = _failure e }
      Right u -> set \st' -> S._dashboard st' { userSearchResult = _success u }

  transfer set st { from, to, value, paymentNote } = do
    set \st' -> S._dashboard st' { transferResult = _loading unit :: RemoteData _ _ _ _ }
    let
      task :: ExceptV (S.ErrTokenTransfer + ()) _ _
      task = env.transfer st.privKey (unsafePartial $ unsafeAddrFromString from) (unsafePartial $ unsafeAddrFromString to) value paymentNote
    result <- runExceptT $ task
    case result of
      Left e -> set \st' -> S._dashboard st' { transferResult = _failure e }
      Right h -> set \st' -> S._dashboard st' { transferResult = _success h }

--------------------------------------------------------------------------------
type EitherV e a = Either (Variant e) a

type RemoteDataV e a = RemoteData Unit Unit (Variant e) a

subscribeRemoteData
  :: forall e a m
   . Monad m
  => (RemoteDataV e a -> m Unit)
  -> m (EitherV e a)
  -> m (EitherV e a)
subscribeRemoteData setCb comp = do
  setCb $ _loading unit
  result <- comp
  setCb $ either _failure _success result
  pure result

--------------------------------------------------------------------------------
subscribeRemoteReport
  :: forall e a m
   . Monad m
  => Env.Env m
  -> (RemoteReport e a -> m Unit)
  -> m (Either e a)
  -> Int
  -> m (Either e a)
subscribeRemoteReport { getTimestamp } setCb comp retry = do
  startTime <- getTimestamp
  setCb $ _loading { timestamp: startTime, retry }
  result <- comp
  endTime <- getTimestamp
  setCb case result of
    Left e -> _failure { error: e, timestamp: endTime, retry }
    Right d -> _success { data: d, timestamp: endTime, retry }
  pure result

subscribeRemoteReport_
  :: forall e a m
   . Monad m
  => Env.Env m
  -> (RemoteReport e a -> m Unit)
  -> m (Either e a)
  -> m (Either e a)
subscribeRemoteReport_ env sub comp = subscribeRemoteReport env sub comp 0

--------------------------------------------------------------------------------
type RetryConfig =
  { delay :: Int
  }

retryUntil
  :: forall m e a
   . Monad m
  => Env.Env m
  -> (Int -> RetryConfig)
  -> (Either e a -> Int -> Boolean)
  -> Int
  -> (Int -> m (Either e a))
  -> m (Either e a)
retryUntil env@{ sleep } getCfg pred retry mkCompu = do
  let
    { delay } = getCfg retry
  result <- mkCompu retry
  let
    newRetry = retry + 1
  if pred result retry then
    pure result
  else do
    sleep delay
    retryUntil env getCfg pred newRetry mkCompu
