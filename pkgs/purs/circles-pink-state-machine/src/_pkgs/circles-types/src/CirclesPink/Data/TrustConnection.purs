module CirclesPink.Data.TrustConnection where

import Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.TrustState (TrustState)
import CirclesPink.GenerateTSD.Wrappers as W
import Data.FpTs.Pair as FpTs
import Data.Generic.Rep (class Generic, Argument, Constructor, Product)
import Data.IxGraph (class Indexed)
import Data.Pair (Pair)
import FpTs.Class (class FpTs, fromFpTs, toFpTs)
import PursTsGen (class ToTsDef, class ToTsType, genericToTsDef)
import PursTsGen.Class.ToPursType (class ToPursType)
import PursTsGen.Lang.PureScript.Type as PS
import PursTsGen.Lang.TypeScript.DSL as TS
import Undefined (undefined)

data TrustConnection = TrustConnection (Pair Address) TrustState

derive instance eq :: Eq TrustConnection
derive instance ord :: Ord TrustConnection

instance indexed :: Indexed (Pair Address) TrustConnection where
  getIndex (TrustConnection conn _) = conn

instance genericTrustconnection ::
  Generic TrustConnection
     (Constructor "TrustConnection" (Product (Argument (W.Pair Address)) (Argument TrustState)) )
      where
  from = undefined
  to = undefined

instance toTsTypeDefTrustConnection :: ToTsDef TrustConnection where
  toTsDef = genericToTsDef "TrustConnection"

instance toTsTypeTrustConnection :: ToTsType TrustConnection where
  toTsType _ = TS.mkType_ $ TS.qualName "CirclesPink_Data_TrustConnection" "TrustConnection"

instance toPursTypeTrustConnection :: ToPursType TrustConnection where
  toPursType _ = PS.mkType (PS.qualName "CirclesPink.Data.TrustConnection" "TrustConnection") [  ]


--------------------------------------------------------------------------------

newtype TsTrustConnection = TsTrustConnection { conn :: FpTs.Pair Address, trustState :: TrustState }

instance fpTs :: FpTs TrustConnection TsTrustConnection where
  toFpTs (TrustConnection conn trustState) = TsTrustConnection { conn: toFpTs conn, trustState }
  fromFpTs (TsTrustConnection { conn, trustState }) = TrustConnection (fromFpTs conn) trustState