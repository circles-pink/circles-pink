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
  , unTrustState
  )
  where

import Prelude

import Data.Newtype (class Newtype)
import Data.Newtype as NT
import Data.Variant (Variant, inj, match)
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

derive instance Newtype TrustState _
derive instance Eq TrustState
derive instance Ord TrustState
derive newtype instance Show TrustState

instance ToTsDef TrustState where
  toTsDef _ = defaultToTsDef trustState []

instance ToTsType TrustState where
  toTsType _ = defaultToTsType trustState []

instance ToPursType TrustState where
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

unTrustState :: TrustState -> TrustState'
unTrustState = NT.unwrap