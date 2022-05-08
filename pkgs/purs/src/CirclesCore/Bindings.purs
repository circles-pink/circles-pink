module CirclesCore.Bindings
  ( Account
  , Balance(..)
  , CirclesCore
  , CirclesCore_(..)
  , CoreSafe(..)
  , CoreToken(..)
  , CoreTrust(..)
  , CoreUser(..)
  , Fn2Promise
  , Fn3Promise
  , Options
  , Provider
  , TrustIsTrustedResult(..)
  , User(..)
  , Web3
  , checkResult
  , convertCore
  , intToBN
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , privKeyToAccount
  , sendTransaction
  , strToBN
  , unsafeSampleCore
  ) where

import Prelude
import CirclesCore.ApiResult (ApiResult)
import Control.Promise (Promise)
import Data.BigInt (BigInt)
import Data.Function.Uncurried (Fn2, Fn3)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff.Compat (EffectFnAff)
import Foreign (Foreign)
import Structural (class Structural, checkStructural)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
foreign import data Provider :: Type

foreign import data Web3 :: Type

foreign import data CirclesCore :: Type

foreign import data Account :: Type

instance structuralAccount :: Structural Account

--------------------------------------------------------------------------------
newtype User
  = User
  { id :: Int
  , username :: String
  , safeAddress :: String
  , avatarUrl :: String
  }

derive instance newtypeUser :: Newtype User _

derive newtype instance structuralUser :: Structural User

--------------------------------------------------------------------------------
newtype TrustIsTrustedResult
  = TrustIsTrustedResult
  { trustConnections :: Int
  , isTrusted :: Boolean
  }

derive instance newtypeTrustIsTrustedResult :: Newtype TrustIsTrustedResult _

derive newtype instance structuralTrustIsTrustedResult :: Structural TrustIsTrustedResult

--------------------------------------------------------------------------------
newtype Balance
  = Balance
  { length :: Int
  , negative :: Int
  , red :: Boolean
  , words :: Array Int
  }

derive instance newtypeBalance :: Newtype Balance _

derive newtype instance structuralBalance :: Structural Balance

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------
foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

foreign import privKeyToAccount :: Web3 -> String -> Effect Account

foreign import sendTransaction :: Web3 -> String -> String -> Effect Unit

foreign import strToBN :: String -> Effect Balance

foreign import intToBN :: Int -> Effect Balance

--------------------------------------------------------------------------------
-- FFI / newCirclesCore
--------------------------------------------------------------------------------
type Options
  = { apiServiceEndpoint :: String
    , graphNodeEndpoint :: String
    , hubAddress :: String
    , proxyFactoryAddress :: String
    , relayServiceEndpoint :: String
    , safeMasterAddress :: String
    , subgraphName :: String
    , databaseSource :: String
    }

foreign import newCirclesCore :: Web3 -> Options -> Effect CirclesCore

newtype CirclesCore_
  = CirclesCore_
  { user :: CoreUser
  , trust :: CoreTrust
  , safe :: CoreSafe
  , token :: CoreToken
  }

derive instance newtypeCirclesCore_ :: Newtype CirclesCore_ _

derive newtype instance structuralCirclesCore_ :: Structural CirclesCore_

--------------------------------------------------------------------------------
newtype CoreUser
  = CoreUser
  { register ::
      Fn2Promise Account
        { nonce :: BigInt
        , safeAddress :: String
        , username :: String
        , email :: String
        }
        Boolean
  , resolve ::
      Fn2Promise Account
        { addresses :: Array String
        , userNames :: Array String
        }
        (ApiResult (Array User))
  , search ::
      Fn2Promise Account { query :: String }
        (ApiResult (Array User))
  }

derive instance newtypeCoreUser :: Newtype CoreUser _

derive newtype instance structuralCoreUser :: Structural CoreUser

--------------------------------------------------------------------------------
newtype CoreSafe
  = CoreSafe
  { deploy :: Fn2Promise Account { safeAddress :: String } Boolean
  , isFunded :: Fn2Promise Account { safeAddress :: String } Boolean
  , predictAddress :: Fn2Promise Account { nonce :: BigInt } String
  , prepareDeploy :: Fn2Promise Account { nonce :: BigInt } String
  , getSafeStatus ::
      Fn2Promise Account { safeAddress :: String }
        { isCreated :: Boolean
        , isDeployed :: Boolean
        }
  }

derive instance newtypeCoreSafe :: Newtype CoreSafe _

derive newtype instance structuralCoreSafe :: Structural CoreSafe

--------------------------------------------------------------------------------
newtype CoreToken
  = CoreToken
  { deploy :: Fn2Promise Account { safeAddress :: String } String
  , getBalance :: Fn2Promise Account { safeAddress :: String } Balance
  , checkUBIPayout :: Fn2Promise Account { safeAddress :: String } Balance
  , requestUBIPayout :: Fn2Promise Account { safeAddress :: String } String
  , transfer ::
      Fn2Promise Account
        { from :: String
        , to :: String
        , value :: Balance
        , paymentNote :: String
        }
        String
  }

derive instance newtypeCoreToken :: Newtype CoreToken _

derive newtype instance structuralCoreToken :: Structural CoreToken

--------------------------------------------------------------------------------
newtype CoreTrust
  = CoreTrust
  { isTrusted ::
      Fn2Promise Account
        { safeAddress :: String, limit :: Int }
        TrustIsTrustedResult
  , getNetwork ::
      Fn2Promise Account { safeAddress :: String }
        ( Array
            { isIncoming :: Boolean
            , isOutgoing :: Boolean
            , limitPercentageIn :: Int
            , limitPercentageOut :: Int
            , mutualConnections :: Array Foreign
            , safeAddress :: String
            }
        )
  , addConnection ::
      Fn2Promise Account
        { user :: String, canSendTo :: String }
        String
  , removeConnection ::
      Fn2Promise Account
        { user :: String, canSendTo :: String }
        String
  }

derive instance newtypeCoreTrust :: Newtype CoreTrust _

derive newtype instance structuralCoreTrust :: Structural CoreTrust

--------------------------------------------------------------------------------
foreign import mkCirclesCore :: Web3 -> Options -> Effect CirclesCore_

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
foreign import unsafeSampleCore :: CirclesCore -> Account -> EffectFnAff Unit

convertCore :: CirclesCore -> CirclesCore_
convertCore = unsafeCoerce

type Fn2Promise a1 a2 b
  = Fn2 a1 a2 (Promise b)

type Fn3Promise a1 a2 a3 b
  = Fn3 a1 a2 a3 (Promise b)

--------------------------------------------------------------------------------
checkResult :: Unit
checkResult = checkStructural (Proxy :: _ CirclesCore_)
