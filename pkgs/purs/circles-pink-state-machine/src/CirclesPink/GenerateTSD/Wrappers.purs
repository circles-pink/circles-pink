module CirclesPink.GenerateTSD.Wrappers
  ( NeighborConnectivity(..)
  , type (:*:)
  , type (:+:)
  ) where

import Data.Generic.Rep (class Generic, Argument, Constructor, Product, Sum)
import Data.IxGraph as Data.IxGraph
import Data.Newtype (class Newtype)
import Data.Typelevel.Undefined (undefined)
import PursTsGen (class ToTsDef, genericToTsDef, toTsType)
import PursTsGen.Class.ToPursType (class ToPursType, toPursType)
import PursTsGen.Class.ToTsType (class ToTsType)
import PursTsGen.Lang.PureScript.Type as PS
import PursTsGen.Lang.TypeScript.DSL as TS
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

infixr 6 type Sum as :+:
infixl 7 type Product as :*:

--------------------------------------------------------------------------------

newtype NeighborConnectivity a = NeighborConnectivity (Data.IxGraph.NeighborConnectivity a)

derive instance Newtype (NeighborConnectivity a) _

instance g ::
  Generic (NeighborConnectivity a)
    ( (Constructor "JustOutgoing" (Argument a))
        :+: (Constructor "JustIncoming" (Argument a))
        :+: (Constructor "MutualOutAndIn" (Argument a))
    ) where
  from = undefined
  to = undefined

instance (ToTsType a) => ToTsType (NeighborConnectivity a) where
  toTsType _ = TS.mkType (TS.qualName "Data_IxGraph" "NeighborConnectivity")
    [ toTsType (Proxy :: _ a) ]

instance (ToPursType a, ToTsType a) => ToTsDef (NeighborConnectivity a) where
  toTsDef = genericToTsDef "NeighborConnectivity"

instance (ToPursType a) => ToPursType (NeighborConnectivity a) where
  toPursType _ = PS.mkType (PS.qualName "Data_IxGraph" "NeighborConnectivity")
    [ toPursType (Proxy :: _ a)
    ]

