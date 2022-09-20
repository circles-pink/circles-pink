module CirclesPink.GenerateTSD.Wrappers
  ( IxGraph(..)
  , NeighborConnectivity(..)
  , type (:*:)
  , type (:+:)
  ) where

import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, Product, Sum)
import Data.IxGraph as Data.IxGraph
import Data.Newtype (class Newtype)
import Data.Typelevel.Undefined (undefined)
import PursTsGen (class ToTsDef, genericToTsDef, toTsType)
import PursTsGen.Class.ToPursType (class ToPursType, toPursType)
import PursTsGen.Class.ToTsType (class ToTsType)
import PursTsGen.Data.ABC (A, B, C)
import PursTsGen.Lang.PureScript.Type as PS
import PursTsGen.Lang.TypeScript.DSL as TS
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

infixr 6 type Sum as :+:
infixl 7 type Product as :*:

--------------------------------------------------------------------------------

newtype IxGraph id e n = IxGraph (Data.IxGraph.IxGraph id e n)

instance ToTsDef (IxGraph A B C) where
  toTsDef _ = pure $ TS.typeDef (TS.name "IxGraph") []
    $ TS.opaque (TS.qualName "Data_IxGraph" "IxGraph")
    $ TS.Name <$> [ "Id", "E", "N" ]

instance (ToTsType id, ToTsType e, ToTsType n) => ToTsType (IxGraph id e n) where
  toTsType _ = TS.mkType (TS.qualName "Data_IxGraph" "IxGraph") $
    [ toTsType (Proxy :: _ id)
    , toTsType (Proxy :: _ e)
    , toTsType (Proxy :: _ n)
    ]

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

