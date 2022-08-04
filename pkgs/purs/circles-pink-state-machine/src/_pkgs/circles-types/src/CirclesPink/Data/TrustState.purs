module CirclesPink.Data.TrustState
  ( TrustState'
  , TrustState(..)
  , initTrusted
  , initUntrusted
  , isLoadingTrust
  , isLoadingUntrust
  , isPendingTrust
  , isPendingUntrust
  , isTrusted
  , isUntrusted
  , next
  , unwrap
  )
  where

import Prelude

import Data.Newtype (class Newtype)
import Data.Newtype as NT
import Data.Variant (Variant, inj, match)
import FpTs.Class (class FpTs)
import PursTsGen (class ToTsDef, class ToTsType, PursType(..), defaultToPursType, defaultToTsDef, defaultToTsType)
import PursTsGen.Class.ToPursType (class ToPursType)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
newtype TrustState = TrustState TrustState'

type TrustState' = Variant
  ( untrusted :: Unit -- 0
  , loadingTrust :: Unit -- 1
  , pendingTrust :: Unit -- 2
  , trusted :: Unit -- 3
  , loadingUntrust :: Unit -- 4
  , pendingUntrust :: Unit -- 5
  )

derive instance newtypeTrustState :: Newtype TrustState _
derive instance trustStateEq :: Eq TrustState
derive instance ordTrustState :: Ord TrustState
derive newtype instance showTrustState :: Show TrustState

instance fpTs :: FpTs TrustState TrustState where
  toFpTs = identity
  fromFpTs = identity

instance toTsDef :: ToTsDef TrustState where
  toTsDef _ = defaultToTsDef trustState []

instance toTsTypeTrustState :: ToTsType TrustState where
  toTsType _ = defaultToTsType trustState []

instance toPursTypeTrustState :: ToPursType TrustState where
  toPursType _ = defaultToPursType trustState []

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
initTrusted :: TrustState
initTrusted = TrustState $ inj (Proxy :: _ "trusted") unit

initUntrusted :: TrustState
initUntrusted = TrustState $ inj (Proxy :: _ "untrusted") unit

--------------------------------------------------------------------------------
-- Destructors
--------------------------------------------------------------------------------
isUntrusted :: TrustState -> Boolean
isUntrusted (TrustState ts) = ts == (inj (Proxy :: _ "untrusted") unit)

isTrusted :: TrustState -> Boolean
isTrusted (TrustState ts) = ts == (inj (Proxy :: _ "trusted") unit)

isLoadingTrust :: TrustState -> Boolean
isLoadingTrust (TrustState ts) = ts == (inj (Proxy :: _ "loadingTrust") unit)

isLoadingUntrust :: TrustState -> Boolean
isLoadingUntrust (TrustState ts) = ts == (inj (Proxy :: _ "loadingUntrust") unit)

isPendingTrust :: TrustState -> Boolean
isPendingTrust (TrustState ts) = ts == (inj (Proxy :: _ "pendingTrust") unit)

isPendingUntrust :: TrustState -> Boolean
isPendingUntrust (TrustState ts) = ts == (inj (Proxy :: _ "pendingUntrust") unit)

next :: TrustState -> TrustState
next (TrustState ts) =
  TrustState
    $ match
        { untrusted: \_ -> inj (Proxy :: _ "loadingTrust") unit
        , loadingTrust: \_ -> inj (Proxy :: _ "pendingTrust") unit
        , pendingTrust: \_ -> inj (Proxy :: _ "trusted") unit
        , trusted: \_ -> inj (Proxy :: _ "loadingUntrust") unit
        , loadingUntrust: \_ -> inj (Proxy :: _ "pendingUntrust") unit
        , pendingUntrust: \_ -> inj (Proxy :: _ "untrusted") unit
        }
        ts

--------------------------------------------------------------------------------

trustState :: PursType
trustState = PursType "CirclesPink_Data_TrustState" "TrustState"

unwrap :: TrustState -> TrustState'
unwrap = NT.unwrap