module CirclesPink.GenerateTSD.Modules where

import Prelude

import CirclesPink.Data.Address as CirclesPink.Data.Address
import CirclesPink.Data.TrustConnection as CirclesPink.Data.TrustConnection
import CirclesPink.Data.TrustNode as CirclesPink.Data.TrustNode
import CirclesPink.GenerateTSD.Class (class ToTsType, cla, ins, toTsType, typ, val, val')
import CirclesPink.GenerateTSD.TypeClasses (ClassOrd, ORD(..))
import Data.ABC (A, B, C)
import Data.Either as Data.Either
import Data.Graph.Errors (ErrNeighborNodes)
import Data.IxGraph as Data.IxGraph
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Nullable as Data.Nullable
import Data.Ord as Data.Ord
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Language.TypeScript.DTS as DTS
import PursTs (pursModule)
import Type.Proxy (Proxy(..))

moduleMap :: Map String (String /\ String)
moduleMap = modules <#> (fst >>> pursModule) # M.fromFoldable

_Ord :: forall a. Ord a => ToTsType a => a -> DTS.Type
_Ord a = DTS.TypeConstructor (DTS.QualName (Just "Data_Ord") "Ord") [ toTsType a ]

modules :: Array (String /\ Array DTS.Declaration)
modules =
  [ "Data.IxGraph" /\
      join
        [ typ (Proxy :: _ (Data.IxGraph.IxGraph A B C)) "IxGraph"
        , val' [ _Ord ORD ] (Data.IxGraph.neighborNodes :: ORD -> _ ORD B C -> _ (_ (ErrNeighborNodes _ ())) _) "neighborNodes"
        ]

  , "CirclesPink.Data.Address" /\
      join
        [ typ (Proxy :: _ (CirclesPink.Data.Address.Address)) "Address"
        , ins (_Ord (Proxy :: _ CirclesPink.Data.Address.Address)) "ordAddress"
        ]

  , "CirclesPink.Data.TrustNode" /\
      join
        [ typ (Proxy :: _ (CirclesPink.Data.TrustNode.TrustNode)) "TrustNode"
        , val (CirclesPink.Data.TrustNode.unwrap) "unwrap"
        , ins (_Ord (Proxy :: _ CirclesPink.Data.TrustNode.TrustNode)) "ordTrustNode"
        ]

  , "CirclesPink.Data.TrustConnection" /\
      join
        [ typ (Proxy :: _ (CirclesPink.Data.TrustConnection.TrustConnection)) "TrustConnection"
        ]

  , "Data.Either" /\
      join
        [ typ (Proxy :: _ (Data.Either.Either A B)) "Either"
        , val (Data.Either.either :: _ -> _ -> _ A B -> C) "either"
        , val (Data.Either.hush :: _ A B -> _) "hush"
        ]

  , "Data.Maybe" /\
      join
        [ typ (Proxy :: _ (Data.Maybe.Maybe A)) "Maybe"
        , val (Data.Maybe.maybe :: _ -> (A -> B) -> _) "maybe"
        ]

  , "Data.Ord" /\
      join
        [ cla (Proxy :: _ (Data.Ord.Ord ORD => Unit)) (Proxy :: _ (ClassOrd A)) "Ord"
        ]

  , "Data.Nullable" /\
      join
        [ val (Data.Nullable.toNullable :: _ A -> _) "toNullable"
        ]

  --        "CirclesPink.GenerateTSD.SampleModule" /\
  --       [ typ (Proxy :: _ (CirclesPink.GenerateTSD.SampleModule.Baz)) "Baz"
  --       , typ (Proxy :: _ (CirclesPink.GenerateTSD.SampleModule.Vielleicht A)) "Vielleicht"
  --       , val (CirclesPink.GenerateTSD.SampleModule.Foo) "Foo"
  --       , val (CirclesPink.GenerateTSD.SampleModule.Bar) "Bar"
  --       , val (CirclesPink.GenerateTSD.SampleModule.gravity) "gravity"
  --       , val (CirclesPink.GenerateTSD.SampleModule.myName) "myName"
  --       , val (CirclesPink.GenerateTSD.SampleModule.isOff) "isOff"
  --       , val (CirclesPink.GenerateTSD.SampleModule.someItems) "someItems"
  --       , val (CirclesPink.GenerateTSD.SampleModule.user) "user"
  --       , val (CirclesPink.GenerateTSD.SampleModule.add) "add"
  --       , val (CirclesPink.GenerateTSD.SampleModule.fromBaz) "fromBaz"
  --       , val (CirclesPink.GenerateTSD.SampleModule.myNum) "myNum"
  --       , val (CirclesPink.GenerateTSD.SampleModule.myNumVar) "myNumVar"
  --       , val (CirclesPink.GenerateTSD.SampleModule.someMaybe) "someMaybe"
  --       ]
  --   , "Simple.Data.Maybe" /\
  --       [ val (Simple.Data.Maybe.Just :: A -> _) "Just"
  --       , val (Simple.Data.Maybe.Nothing :: _ A) "Nothing"
  --       , val (Simple.Data.Maybe.maybe :: _ -> (A -> B) -> _) "maybe"
  --       , val (Simple.Data.Maybe.maybe' :: _ -> (A -> B) -> _) "maybe'"
  --       , val (Simple.Data.Maybe.bind :: _ A -> (_ -> _ B) -> _) "bind"
  --       , val (Simple.Data.Maybe.bindFlipped :: (_ -> _ B) -> _ A -> _) "bindFlipped"
  --       , val (Simple.Data.Maybe.map :: (A -> B) -> _) "map"
  --       , val (Simple.Data.Maybe.pure :: A -> _) "pure"
  --       , val (Simple.Data.Maybe.eq :: _ -> _ A -> _) "eq"
  --       ]

  --   , "Simple.Data.Number" /\
  --       [ val (Simple.Data.Number.eq) "eq"
  --       ]
  ]

