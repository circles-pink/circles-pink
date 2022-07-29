module CirclesPink.Garden.EnvControlTest
  -- ( TestEnvM
  -- , TestEnvT
  -- , liftEnv
  -- , runTestEnvM
  -- , runTestEnvT
  -- , testEnv
  -- ) 
  where

import Prelude

import CirclesPink.Data.Address (sampleAddress, sampleSafeAddress)
import CirclesPink.Data.PrivateKey (sampleKey)
import CirclesPink.Data.PrivateKey as P
import CirclesPink.Garden.StateMachine.Control.Class.TestScriptT (TestScriptT)
import CirclesPink.Garden.StateMachine.Control.EnvControl (StorageType(..), _errNoStorage, EnvControl)
import Control.Monad.Except (mapExceptT, throwError)
import Data.BN as B
import Data.Newtype (wrap)
import Data.Variant (inj)
import Type.Proxy (Proxy(..))
import Web3 (Hash(..), Message(..), SignatureObj(..))

testEnv :: forall m. Monad m => EnvControl (TestScriptT m)
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
  , trustGetNetwork: \_ _ -> pure []
  , privKeyToSafeAddress: \_ -> pure sampleSafeAddress
  , getSafeStatus: \_ -> pure { isCreated: true, isDeployed: true }
  , deploySafe: \_ -> pure unit
  , deployToken: \_ -> pure ""
  , isFunded: \_ -> pure true
  , addTrustConnection: \_ _ _ -> pure ""
  , removeTrustConnection: \_ _ _ -> pure ""
  , signChallenge: \_ _ -> pure (SignatureObj { message: Message "", messageHash: Hash "", v: "", r: "", s: "", signature: "" })
  , saveSession: \_ -> pure unit
  , restoreSession: pure P.sampleKey
  , getBalance: \_ _ -> pure $ B.fromDecimalStr "34141123134632464543156"
  , checkUBIPayout: \_ _ -> pure $ B.fromDecimalStr "8943789342768937829897"
  , requestUBIPayout: \_ _ -> pure ""
  , transfer: \_ _ _ _ _ -> pure ""
  , getTimestamp: pure bottom
  , sleep: \_ -> pure unit
  , logInfo: \_ -> pure unit
  , storageSetItem: \_ _ _ _ -> pure unit
  , storageGetItem: \_ _ _ -> throwError $ _errNoStorage SessionStorage
  , storageDeleteItem: \_ _ _ -> pure unit
  , storageClear: \_ -> pure unit
  }

--------------------------------------------------------------------------------

compose2 :: forall a1 a2 z z'. (z -> z') -> (a1 -> a2 -> z) -> (a1 -> a2 -> z')
compose2 g f x1 x2 = f x1 x2 # g

compose3 :: forall a1 a2 a3 z z'. (z -> z') -> (a1 -> a2 -> a3 -> z) -> (a1 -> a2 -> a3 -> z')
compose3 g f x1 x2 x3 = f x1 x2 x3 # g

compose4 :: forall a1 a2 a3 a4 z z'. (z -> z') -> (a1 -> a2 -> a3 -> a4 -> z) -> (a1 -> a2 -> a3 -> a4 -> z')
compose4 g f x1 x2 x3 x4 = f x1 x2 x3 x4 # g

compose5 :: forall a1 a2 a3 a4 a5 z z'. (z -> z') -> (a1 -> a2 -> a3 -> a4 -> a5 -> z) -> (a1 -> a2 -> a3 -> a4 -> a5 -> z')
compose5 g f x1 x2 x3 x4 x5 = f x1 x2 x3 x4 x5 # g

liftEnv :: forall m n. (m ~> n) -> EnvControl m -> EnvControl n
liftEnv f e =
  { apiCheckUserName: compose (mapExceptT f) e.apiCheckUserName
  , apiCheckEmail: compose (mapExceptT f) e.apiCheckEmail
  , generatePrivateKey: f e.generatePrivateKey
  , userRegister: compose2 (mapExceptT f) e.userRegister
  , userSearch: compose2 (mapExceptT f) e.userSearch
  , getSafeAddress: compose (mapExceptT f) e.getSafeAddress
  , safePrepareDeploy: compose (mapExceptT f) e.safePrepareDeploy
  , userResolve: compose (mapExceptT f) e.userResolve
  , getUsers: compose3 (mapExceptT f) e.getUsers
  , coreToWindow: compose (mapExceptT f) e.coreToWindow
  , isTrusted: compose (mapExceptT f) e.isTrusted
  , trustGetNetwork: compose2 (mapExceptT f) e.trustGetNetwork
  , privKeyToSafeAddress: compose (mapExceptT f) e.privKeyToSafeAddress
  , getSafeStatus: compose (mapExceptT f) e.getSafeStatus
  , deploySafe: compose (mapExceptT f) e.deploySafe
  , deployToken: compose (mapExceptT f) e.deployToken
  , isFunded: compose (mapExceptT f) e.isFunded
  , addTrustConnection: compose3 (mapExceptT f) e.addTrustConnection
  , removeTrustConnection: compose3 (mapExceptT f) e.removeTrustConnection
  , saveSession: compose (mapExceptT f) e.saveSession
  , restoreSession: (mapExceptT f) e.restoreSession
  , signChallenge: e.signChallenge
  , getBalance: compose2 (mapExceptT f) e.getBalance
  , checkUBIPayout: compose2 (mapExceptT f) e.checkUBIPayout
  , requestUBIPayout: compose2 (mapExceptT f) e.requestUBIPayout
  , transfer: compose5 (mapExceptT f) e.transfer
  , getTimestamp: f e.getTimestamp
  , sleep: compose f e.sleep
  , logInfo: compose f e.logInfo
  , storageSetItem: \x1 x2 x3 x4 -> e.storageSetItem x1 x2 x3 x4 # mapExceptT f
  , storageGetItem: \x1 x2 x3 -> e.storageGetItem x1 x2 x3 # mapExceptT f
  , storageDeleteItem: \x1 x2 x3 -> e.storageDeleteItem x1 x2 x3 # mapExceptT f
  , storageClear: compose (mapExceptT f) e.storageClear
  }
