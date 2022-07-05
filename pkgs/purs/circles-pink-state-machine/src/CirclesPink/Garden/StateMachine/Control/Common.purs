module CirclesPink.Garden.StateMachine.Control.Common where

import Prelude

import CirclesCore (SafeStatus, TrustNode, User)
import CirclesPink.Garden.StateMachine.Control.Env as Env
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import CirclesPink.Data.PrivateKey (PrivateKey)
import Data.Variant (Variant, default, onMatch)
import Prim.Row (class Nub)
import RemoteData (RemoteData, _failure, _loading, _success)
import RemoteReport (RemoteReport)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

run :: forall t m e a. MonadTrans t => Monad m => ExceptV e m a -> t m (Either (Variant e) a)
run = lift <<< runExceptT

run' :: forall t m e e' a. MonadTrans t => Nub e e' => Monad m => ExceptV e m a -> t m (Either (Variant e') a)
run' = lift <<< runExceptT'

runExceptT' :: forall m e e' a. ExceptV e m a -> m (Either (Variant e') a)
runExceptT' = unsafeCoerce <<< runExceptT

type ActionHandler :: forall k. (k -> Type -> Type) -> k -> Type -> Type -> Row Type -> Type
type ActionHandler t m a s v = ((s -> Variant v) -> t m Unit) -> s -> a -> t m Unit

type ActionHandler' m a s v = ((s -> Variant v) -> m Unit) -> s -> a -> m Unit

--------------------------------------------------------------------------------
type ErrReadyForDeployment r = Env.ErrIsTrusted + Env.ErrIsFunded + r

readyForDeployment :: forall m r. Monad m => Env.Env m -> PrivateKey -> ExceptV (ErrReadyForDeployment r) m Boolean
readyForDeployment { isTrusted, isFunded } privKey = do
  isTrusted' <- isTrusted privKey <#> (unwrap >>> _.isTrusted)
  isFunded' <- isFunded privKey
  pure (isTrusted' || isFunded')

--------------------------------------------------------------------------------
type TaskReturn =
  { user :: User
  , isTrusted :: Boolean
  , trusts :: Array TrustNode
  , safeStatus :: SafeStatus
  , isReady :: Boolean
  }

type ErrLoginTask r = Env.ErrUserResolve
  + Env.ErrGetSafeStatus
  + Env.ErrTrustGetNetwork
  + Env.ErrIsTrusted
  + Env.ErrIsFunded
  + r

loginTask :: forall m r. Monad m => Env.Env m -> PrivateKey -> ExceptV (ErrLoginTask + r) m TaskReturn
loginTask env privKey = do
  user <- env.userResolve privKey
  safeStatus <- env.getSafeStatus privKey
  isTrusted <- env.isTrusted privKey <#> (unwrap >>> _.isTrusted)
  trusts <- if isTrusted then pure [] else env.trustGetNetwork privKey
  isReady' <- readyForDeployment env privKey
  pure { user, isTrusted, trusts, safeStatus, isReady: isReady' }

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

type ErrDeploySafe' r = Env.ErrDeploySafe + Env.ErrGetSafeStatus + r

deploySafe' :: forall m r. Monad m => Env.Env m -> PrivateKey -> ExceptV (ErrDeploySafe' r) m SafeStatus
deploySafe' { deploySafe, getSafeStatus } privKey = do
  _ <- deploySafe privKey
  safeStatus <- getSafeStatus privKey
  pure safeStatus