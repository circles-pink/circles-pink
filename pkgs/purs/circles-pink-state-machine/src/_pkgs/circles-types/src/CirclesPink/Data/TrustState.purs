module CirclesPink.Data.TrustState
  ( TrustState(..)
  , initTrusted
  , initUntrusted
  , isLoadingTrust
  , isLoadingUntrust
  , isPendingTrust
  , isPendingUntrust
  , isTrusted
  , isUntrusted
  , next
  ) where

import Prelude

import CirclesPink.GenerateTSD.Replace as R
import Data.Generic.Rep (class Generic)
import Data.Variant (Variant, inj, match)
import FpTs.Class (class FpTs)
import PursTsGen (class ToTsDef, class ToTsType, genericToTsDef)
import PursTsGen.Class.ToPursType (class ToPursType)
import PursTsGen.Lang.PureScript.Type as PS
import PursTsGen.Lang.TypeScript.DSL as TS
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
newtype TrustState = TrustState
  ( Variant
      ( untrusted :: Unit -- 0
      , loadingTrust :: Unit -- 1
      , pendingTrust :: Unit -- 2
      , trusted :: Unit -- 3
      , loadingUntrust :: Unit -- 4
      , pendingUntrust :: Unit -- 5
      )
  )


derive instance trustStateEq :: Eq TrustState
derive instance ordTrustState :: Ord TrustState
derive newtype instance showTrustState :: Show TrustState

instance fpTs :: FpTs TrustState TrustState where
  toFpTs = identity
  fromFpTs = identity



instance toTsTypeTrustState :: ToTsType TrustState where
  toTsType _ = TS.mkType_ $ TS.qualName "CirclesPink_Data_TrustState" "TrustState"

instance toPursTypeTrustState :: ToPursType TrustState where
  toPursType _ = PS.mkType (PS.qualName "CirclesPink_Data_TrustState" "TrustState") [  ]



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

