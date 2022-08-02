module CirclesPink.GenerateTSD.Modules where

import Prelude

import CirclesPink.Data.Address as CirclesPink.Data.Address
import CirclesPink.Data.TrustConnection as CirclesPink.Data.TrustConnection
import CirclesPink.Data.TrustNode as CirclesPink.Data.TrustNode
import CirclesPink.Data.UserIdent as CirclesPink.Data.UserIdent
import CirclesPink.GenerateTSD.TypeClasses (ClassOrd, ORD(..))
import Data.Either (Either)
import Data.Either as Data.Either
import Data.Generic.Rep (class Generic, Argument, Constructor(..), NoArguments(..), Product, Sum)
import Data.Graph.Errors (ErrNeighborNodes, ErrNeighborhood)
import Data.IxGraph as Data.IxGraph
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe as Data.Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable as Data.Nullable
import Data.Ord as Data.Ord
import Data.Tuple (Tuple, fst)
import Data.Tuple as Data.Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Undefined (undefined)
import Debug.Extra (todo)
import PursTsGen (class ToTsDef, classDef, constructor, defPredicateFn, genericToTsDef, instanceDef, pursModule, toTsType)
import PursTsGen as PT
import PursTsGen.Class.ToPursType (class ToPursType, toPursType)
import PursTsGen.Class.ToTsType (class ToTsType)
import PursTsGen.Data.ABC (A(..), B, C)
import PursTsGen.Lang.PureScript.Type as PS
import PursTsGen.Lang.TypeScript.DSL as TS
import Simple.Data.Array as Simple.Data.Array
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import CirclesPink.GenerateTSD.Replace as R

moduleMap :: Map String (String /\ String)
moduleMap = modules <#> (fst >>> pursModule) # M.fromFoldable

_Ord :: forall a. Ord a => ToTsType a => a -> TS.Type
_Ord a = TS.TypeConstructor (TS.QualName (Just "Data_Ord") "Ord") [ toTsType a ]

modules :: Array (String /\ Array TS.Declaration)
modules =
  [ "Data.IxGraph" /\
      join
        [ R.typeDef "IxGraph" (Proxy :: _ (Data.IxGraph.IxGraph A B C))
        , R.typeDef "NeighborConnectivity" (Proxy :: _ (Data.IxGraph.NeighborConnectivity A))
        , R.value "neighborNodes"
            [ _Ord ORD ]
            (Data.IxGraph.neighborNodes :: ORD -> _ ORD B C -> _ (_ (ErrNeighborNodes ORD ())) _)
        , R.value "neighborhood"
            [ _Ord ORD ]
            (Data.IxGraph.neighborhood :: ORD -> _ ORD B C -> _ (_ (ErrNeighborhood ORD ())) _)

        ]
  , "CirclesPink.Data.Address" /\
      join
        [ typeDef "Address" (Proxy :: _ CirclesPink.Data.Address.Address)
        , instanceDef "ordAddress" (_Ord (Proxy :: _ CirclesPink.Data.Address.Address))
        ]
  , "Simple.Data.Array" /\
      join
        [ value "map" [] (Simple.Data.Array.map :: (A -> B) -> _) ]
  , "CirclesPink.Data.TrustNode" /\
      join
        [ typeDef "TrustNode" (Proxy :: _ CirclesPink.Data.TrustNode.TrustNode)
        , value "unwrap" [] CirclesPink.Data.TrustNode.unwrap
        , instanceDef "ordTrustNode" (_Ord (Proxy :: _ CirclesPink.Data.TrustNode.TrustNode))
        ]
  , "CirclesPink.Data.UserIdent" /\
      join
        [ typeDef "UserIdent" (Proxy :: _ CirclesPink.Data.UserIdent.UserIdent)
        , value "unwrap" [] CirclesPink.Data.UserIdent.unwrap
        , value "getIdentifier" [] CirclesPink.Data.UserIdent.getIdentifier
        ]
  , "CirclesPink.Data.TrustConnection" /\
      join
        [ typeDef "TrustConnection" (Proxy :: _ (CirclesPink.Data.TrustConnection.TrustConnection))
        ]
  , "Data.Either" /\
      join
        [ typeDef "Either" (Proxy :: _ (Data.Either.Either A B))
        , value "either" [] (Data.Either.either :: _ -> _ -> _ A B -> C)
        , value "hush" [] (Data.Either.hush :: _ A B -> _)
        , defPredicateFn "isLeft" [] (Data.Either.isLeft :: Either A B -> Boolean)
            (TS.mkType (TS.qualName_ "Either_Left") [ toTsType A ])
        , defPredicateFn "isRight" [] (Data.Either.isRight :: _ A B -> _)
            (TS.mkType (TS.qualName_ "Either_Right") [ toTsType B ])
        ]
  , "Data.Tuple" /\
      join
        [ typeDef "Tuple" (Proxy :: _ (Data.Tuple.Tuple A B))
        ]
  , "Data.Maybe" /\
      join
        [ typeDef "Maybe" (Proxy :: _ (Data.Maybe.Maybe A))
        , value "maybe" [] (Data.Maybe.maybe :: _ -> (A -> B) -> _)
        ]
  , "Data.Ord" /\
      join
        [ classDef "Ord" (Proxy :: _ (Data.Ord.Ord ORD => Unit)) (Proxy :: _ (ClassOrd A))
        ]
  , "Data.Nullable" /\
      join
        [ value "toNullable" [] (Data.Nullable.toNullable :: _ A -> _)
        ]

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