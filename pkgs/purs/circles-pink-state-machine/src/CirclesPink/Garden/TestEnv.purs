module CirclesPink.Garden.TestEnv
  ( TestEnvM
  , TestEnvT
  , liftEnv
  , runTestEnvM
  , runTestEnvT
  , testEnv
  )
  where

import Prelude

import CirclesPink.Data.Address (sampleAddress, sampleSafeAddress)
import CirclesPink.Data.PrivateKey (sampleKey)
import CirclesPink.Data.PrivateKey as P
import CirclesPink.Garden.StateMachine.Control.Env as Env
import Control.Monad.Except (class MonadTrans, ExceptT, lift, mapExceptT, throwError)
import Control.Monad.State (StateT, evalState, evalStateT)
import Data.BN as B
import Data.Identity (Identity)
import Data.Newtype (wrap)
import Data.Variant (inj)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

type TestEnvT m = StateT {} m

type TestEnvM = TestEnvT Identity

runTestEnvM :: forall a. TestEnvM a -> a
runTestEnvM x = evalState x {}

runTestEnvT :: forall m a. Monad m => TestEnvT m a -> m a
runTestEnvT x = evalStateT x {}

testEnv :: forall m. Monad m => Env.Env (TestEnvT m)
testEnv =
  { apiCheckUserName: \_ -> pure { isValid: true }
  , apiCheckEmail: \_ -> pure { isValid: true }
  , generatePrivateKey: pure sampleKey
  , userRegister: \_ _ -> pure unit
  , userSearch: \_ _ -> pure []
  , getSafeAddress: \_ -> pure sampleAddress
  , safePrepareDeploy: \_ -> pure sampleAddress
  , userResolve: \pk ->
      if pk == sampleKey then pure { id: 0, username: "", safeAddress: sampleAddress, avatarUrl: "" }
      else throwError $ inj (Proxy :: _ "errUserNotFound") { safeAddress: sampleSafeAddress }
  , getUsers: \_ _ _ -> pure []
  , coreToWindow: \_ -> pure unit
  , isTrusted: \_ -> pure $ wrap { isTrusted: true, trustConnections: 3 }
  , trustGetNetwork: \_ -> pure []
  , getSafeStatus: \_ -> pure { isCreated: true, isDeployed: true }
  , deploySafe: \_ -> pure unit
  , deployToken: \_ -> pure ""
  , isFunded: \_ -> pure true
  , addTrustConnection: \_ _ _ -> pure ""
  , removeTrustConnection: \_ _ _ -> pure ""
  , saveSession: \_ -> pure unit
  , restoreSession: pure P.sampleKey
  , getBalance: \_ _ -> pure $ B.fromDecimalStr "34141123134632464543156"
  , checkUBIPayout: \_ _ -> pure $ B.fromDecimalStr "8943789342768937829897"
  , requestUBIPayout: \_ _ -> pure ""
  , transfer: \_ _ _ _ _ -> pure ""
  , getTimestamp: pure bottom
  , sleep: \_ -> pure unit
  }

--------------------------------------------------------------------------------

liftVal :: forall e m a t. MonadTrans t => Monad m => ExceptT e m a -> ExceptT e (t m) a
liftVal f = f # mapExceptT lift

liftFn1 :: forall a1 e m a t. MonadTrans t => Monad m => (a1 -> ExceptT e m a) -> a1 -> (ExceptT e (t m) a)
liftFn1 f x1 = f x1 # mapExceptT lift

liftFn2 :: forall a1 a2 e m a t. MonadTrans t => Monad m => (a1 -> a2 -> ExceptT e m a) -> a1 -> a2 -> (ExceptT e (t m) a)
liftFn2 f x1 x2 = f x1 x2 # mapExceptT lift

liftFn3 :: forall a1 a2 a3 e m a t. MonadTrans t => Monad m => (a1 -> a2 -> a3 -> ExceptT e m a) -> a1 -> a2 -> a3 -> (ExceptT e (t m) a)
liftFn3 f x1 x2 x3 = f x1 x2 x3 # mapExceptT lift

liftFn4 :: forall a1 a2 a3 a4 e m a t. MonadTrans t => Monad m => (a1 -> a2 -> a3 -> a4 -> ExceptT e m a) -> a1 -> a2 -> a3 -> a4 -> (ExceptT e (t m) a)
liftFn4 f x1 x2 x3 x4 = f x1 x2 x3 x4 # mapExceptT lift

liftFn5 :: forall a1 a2 a3 a4 a5 e m a t. MonadTrans t => Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> ExceptT e m a) -> a1 -> a2 -> a3 -> a4 -> a5 -> (ExceptT e (t m) a)
liftFn5 f x1 x2 x3 x4 x5 = f x1 x2 x3 x4 x5 # mapExceptT lift

liftEnv :: forall t m. MonadTrans t => Monad m => Env.Env m -> Env.Env (t m)
liftEnv e =
  { apiCheckUserName: liftFn1 e.apiCheckUserName
  , apiCheckEmail: liftFn1 e.apiCheckEmail
  , generatePrivateKey: lift e.generatePrivateKey
  , userRegister: liftFn2 e.userRegister
  , userSearch: liftFn2 e.userSearch
  , getSafeAddress: liftFn1 e.getSafeAddress
  , safePrepareDeploy: liftFn1 e.safePrepareDeploy
  , userResolve: liftFn1 e.userResolve
  , getUsers: liftFn3 e.getUsers
  , coreToWindow: liftFn1 e.coreToWindow
  , isTrusted: liftFn1 e.isTrusted
  , trustGetNetwork: liftFn1 e.trustGetNetwork
  , getSafeStatus: liftFn1 e.getSafeStatus
  , deploySafe: liftFn1 e.deploySafe
  , deployToken: liftFn1 e.deployToken
  , isFunded: liftFn1 e.isFunded
  , addTrustConnection: liftFn3 e.addTrustConnection
  , removeTrustConnection: liftFn3 e.removeTrustConnection
  , saveSession: liftFn1 e.saveSession
  , restoreSession: liftVal e.restoreSession
  , getBalance: liftFn2 e.getBalance
  , checkUBIPayout: liftFn2 e.checkUBIPayout
  , requestUBIPayout: liftFn2 e.requestUBIPayout
  , transfer: liftFn5 e.transfer
  , getTimestamp: lift e.getTimestamp
  , sleep: lift <<< e.sleep
  }

--------------------------------------------------------------------------------