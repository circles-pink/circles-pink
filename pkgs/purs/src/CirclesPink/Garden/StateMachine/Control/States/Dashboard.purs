module CirclesPink.Garden.StateMachine.Control.States.Dashboard
  ( dashboard
  , fetchUsersBinarySearch
  ) where

import Prelude

import CirclesCore (User, TrustNode)
import CirclesCore as CC
import CirclesPink.Garden.StateMachine.Control.Common (ActionHandler')
import CirclesPink.Garden.StateMachine.Control.Env as Env
import CirclesPink.Garden.StateMachine.State as S
import CirclesPink.Garden.StateMachine.State.Dashboard (Trust, _loadingTrust, _loadingUntrust, _pendingTrust, _pendingUntrust, _trusted, _untrusted)
import Control.Monad.Except (ExceptT(..), catchError, mapExceptT, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (lift)
import Convertable (convert)
import Data.Array (catMaybes, drop, find, take)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush, isRight)
import Data.Int (floor, toNumber)
import Data.Map (Map, lookup, update)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (length)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant, default, onMatch)
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
          # retryUntil env (const { delay: 15000 }) (\_ _ -> false) 0

        pure unit

  syncTrusts set st i =
    let
      mapTrust :: Array User -> Map W3.Address Trust -> TrustNode -> W3.Address /\ Trust
      mapTrust foundUsers oldTrusts t = convert t.safeAddress /\ user
        where
        trustState = case lookup (convert t.safeAddress) oldTrusts of
          Nothing -> if t.isIncoming then _trusted else _untrusted
          Just { trustState: oldTrustState } ->
            if t.isIncoming then
              ( default oldTrustState # onMatch
                  { "pendingTrust": \_ -> _trusted
                  , "loadingTrust": \_ -> _trusted
                  }
              ) oldTrustState
            else
              ( default oldTrustState # onMatch
                  { "pendingUntrust": \_ -> _untrusted
                  , "loadingUntrust": \_ -> _untrusted
                  }
              ) oldTrustState
        user =
          { isOutgoing: t.isOutgoing
          , user: find (\u -> u.safeAddress == t.safeAddress) foundUsers
          , trustState
          }
    in
      do
        trusts <-
          env.trustGetNetwork st.privKey
            # (\x -> subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustsResult = r }) x i)
            # dropError
        users <-
          fetchUsersBinarySearch env st.privKey (map (convert <<< _.safeAddress) trusts)
            # dropError
        let
          foundUsers = catMaybes $ map hush users
        lift $ set \st' -> S._dashboard st' { trusts = trusts <#> mapTrust foundUsers st'.trusts # M.fromFoldable }

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
        lift $ set \st' ->
          S._dashboard st' { trusts = update (\t -> pure $ t { trustState = _loadingTrust }) (convert addr) st'.trusts }
        _ <-
          env.addTrustConnection st.privKey addr st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustAddResult = insert u r st.trustAddResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # dropError
        lift $ set \st' -> S._dashboard st' { trusts = update (\t -> pure $ t { trustState = _pendingTrust }) (convert addr) st'.trusts }
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
        lift $ set \st' ->
          S._dashboard st' { trusts = update (\t -> pure $ t { trustState = _loadingUntrust }) (convert addr) st'.trusts }
        _ <-
          env.removeTrustConnection st.privKey addr st.user.safeAddress
            # subscribeRemoteReport env (\r -> set \st' -> S._dashboard st' { trustRemoveResult = insert u r st.trustRemoveResult })
            # retryUntil env (const { delay: 10000 }) (\r n -> n == 10 || isRight r) 0
            # dropError
        lift $ set \st' -> S._dashboard st' { trusts = update (\t -> pure $ t { trustState = _pendingUntrust }) (convert addr) st'.trusts }
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
            # retryUntil env (const { delay: 15000 }) (\_ _ -> false) 0

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
  -> ExceptT e m a
  -> Int
  -> ExceptT e m a
subscribeRemoteReport { getTimestamp } setCb comp retry = ExceptT do
  startTime <- getTimestamp
  setCb $ _loading { timestamp: startTime, retry, previousData: Nothing :: Maybe a }
  result :: Either e a <- runExceptT comp
  endTime <- getTimestamp
  setCb case result of
    Left e -> _failure { error: e, timestamp: endTime, retry }
    Right d -> _success { data: d, timestamp: endTime, retry }
  pure result

addPreviousData :: forall e a. a -> RemoteReport e a -> RemoteReport e a
addPreviousData pd rp =
  (default rp # onMatch { loading: \x -> _loading $ x { previousData = Just pd } }) (unwrap rp)

-- subscribeRemoteReport_
--   :: forall e a m
--    . Monad m
--   => Env.Env m
--   -> (RemoteReport e a -> m Unit)
--   -> m (Either e a)
--   -> m (Either e a)
-- subscribeRemoteReport_ env sub comp = subscribeRemoteReport env sub comp 0

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
  -> (Int -> ExceptT e m a)
  -> ExceptT e m a
retryUntil env@{ sleep } getCfg pred retry mkCompu = ExceptT do
  let
    { delay } = getCfg retry
  result <- runExceptT $ mkCompu retry
  let
    newRetry = retry + 1
  if pred result retry then
    pure result
  else do
    sleep delay
    runExceptT $ retryUntil env getCfg pred newRetry mkCompu

dropError :: ∀ (t320 ∷ Type -> Type) (t322 ∷ Type) (t331 ∷ Type). Functor t320 ⇒ ExceptT t331 t320 t322 → ExceptT Unit t320 t322
dropError = mapExceptT (\x -> x <#> lmap (const unit))