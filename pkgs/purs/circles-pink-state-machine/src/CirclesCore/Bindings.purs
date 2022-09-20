module CirclesCore.Bindings
  ( Account
  , CirclesCore
  , CirclesCore_(..)
  , CoreOrganization(..)
  , CoreSafe(..)
  , CoreToken(..)
  , CoreTrust(..)
  , CoreUser(..)
  , CoreUtils(..)
  , Fn1Promise
  , Fn2Promise
  , Fn3Promise
  , Fn4Promise
  , Options
  , Provider
  , TrustIsTrustedResult(..)
  , User(..)
  , convertCore
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , privKeyToAccount
  , sendTransaction
  , unsafeSampleCore
  ) where

import Prelude

import CirclesCore.ApiResult (ApiResult)
import Control.Promise (Promise)
import Data.BN (BN)
import Data.BigInt (BigInt)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, Fn1)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff.Compat (EffectFnAff)
import Foreign (Foreign)
import Structural (class Structural)
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (type (|+|))
import Web3 (Web3)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
foreign import data Provider :: Type

foreign import data CirclesCore :: Type

foreign import data Account :: Type

instance Structural Account

--------------------------------------------------------------------------------
newtype User = User
  { id :: Int
  , username :: String
  , safeAddress :: String
  , avatarUrl :: String
  }

derive instance Newtype User _

derive newtype instance Structural User

--------------------------------------------------------------------------------
newtype TrustIsTrustedResult = TrustIsTrustedResult
  { trustConnections :: Int
  , isTrusted :: Boolean
  }

derive instance Newtype TrustIsTrustedResult _

derive newtype instance Structural TrustIsTrustedResult

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------
foreign import newWebSocketProvider :: String -> Effect Provider

foreign import newWeb3 :: Provider -> Effect Web3

foreign import privKeyToAccount :: Web3 -> String -> Effect Account

foreign import sendTransaction :: Web3 -> String -> String -> Effect Unit

--------------------------------------------------------------------------------
-- FFI / newCirclesCore
--------------------------------------------------------------------------------
type Options =
  { apiServiceEndpoint :: String
  , graphNodeEndpoint :: String
  , hubAddress :: String
  , proxyFactoryAddress :: String
  , relayServiceEndpoint :: String
  , safeMasterAddress :: String
  , subgraphName :: String
  , databaseSource :: String
  }

foreign import newCirclesCore :: Web3 -> Options -> Effect CirclesCore

newtype CirclesCore_ = CirclesCore_
  { user :: CoreUser
  , trust :: CoreTrust
  , safe :: CoreSafe
  , token :: CoreToken
  , utils :: CoreUtils
  , organization :: CoreOrganization
  }

derive instance Newtype CirclesCore_ _

--------------------------------------------------------------------------------
newtype CoreUser = CoreUser
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

derive instance Newtype CoreUser _

derive newtype instance Structural CoreUser

--------------------------------------------------------------------------------
newtype CoreSafe = CoreSafe
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

derive instance Newtype CoreSafe _

derive newtype instance Structural CoreSafe

--------------------------------------------------------------------------------
newtype CoreToken = CoreToken
  { deploy :: Fn2Promise Account { safeAddress :: String } String
  , getBalance :: Fn2Promise Account { safeAddress :: String } BN
  , checkUBIPayout :: Fn2Promise Account { safeAddress :: String } BN
  , requestUBIPayout :: Fn2Promise Account { safeAddress :: String } String
  , transfer ::
      Fn2Promise Account
        { from :: String
        , to :: String
        , value :: BN
        , paymentNote :: String
        }
        (String |+| { data :: Foreign })
  , getPaymentNote :: Fn2Promise Account { transactionHash :: String } (Nullable String)
  }

derive instance Newtype CoreToken _

--------------------------------------------------------------------------------
newtype CoreTrust = CoreTrust
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
            , mutualConnections :: Array String
            , safeAddress :: String
            }
        )
  , addConnection ::
      Fn2Promise Account
        { user :: String, canSendTo :: String, limitPercentage :: Number }
        (String |+| { data :: Foreign })
  , removeConnection ::
      Fn2Promise Account
        { user :: String, canSendTo :: String }
        (String |+| { data :: Foreign })
  }

derive instance Newtype CoreTrust _

derive newtype instance Structural CoreTrust

--------------------------------------------------------------------------------
newtype CoreOrganization = CoreOrganization
  { isOrganization :: Fn2Promise Account { safeAddress :: String } Boolean
  }

derive instance Newtype CoreOrganization _
--------------------------------------------------------------------------------
newtype CoreUtils = CoreUtils
  { toFreckles ::
      Fn1Promise
        { value :: String }
        String
  , fromFreckles ::
      Fn1Promise
        { value :: String }
        String
  , requestRelayer ::
      Fn1Promise
        { path :: Array String
        , version :: Int
        , method :: String
        , data ::
            { saltNonce :: BigInt
            , owners :: Array String
            , threshold :: Int
            }
        }
        { safe :: String
        }
  }

derive instance Newtype CoreUtils _

derive newtype instance Structural CoreUtils

--------------------------------------------------------------------------------
foreign import mkCirclesCore :: Web3 -> Options -> Effect CirclesCore_

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
foreign import unsafeSampleCore :: CirclesCore -> Account -> EffectFnAff Unit

convertCore :: CirclesCore -> CirclesCore_
convertCore = unsafeCoerce

type Fn1Promise a1 b = Fn1 a1 (Promise b)

type Fn2Promise a1 a2 b = Fn2 a1 a2 (Promise b)

type Fn3Promise a1 a2 a3 b = Fn3 a1 a2 a3 (Promise b)

type Fn4Promise a1 a2 a3 a4 b = Fn4 a1 a2 a3 a4 (Promise b)

--------------------------------------------------------------------------------
