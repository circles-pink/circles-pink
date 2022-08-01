module CirclesPink.GenerateTSD.Modules where

import Prelude

import CirclesPink.Data.Address as CirclesPink.Data.Address
import CirclesPink.Data.TrustConnection as CirclesPink.Data.TrustConnection
import CirclesPink.Data.TrustNode as CirclesPink.Data.TrustNode
import CirclesPink.GenerateTSD.TypeClasses (ClassOrd, ORD(..))
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
import PursTsGen (class ToTsDef, class ToTsType, classDef, constructor, instanceDef, pursModule, toTsType)
import PursTsGen as PT
import PursTsGen.Class.ToTsType (Constructor(..))
import PursTsGen.Data.ABC (A, B, C)
import PursTsGen.Lang.TypeScript.DSL as TS
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

moduleMap :: Map String (String /\ String)
moduleMap = modules <#> (fst >>> pursModule) # M.fromFoldable

_Ord :: forall a. Ord a => ToTsType a => a -> TS.Type
_Ord a = TS.TypeConstructor (TS.QualName (Just "Data_Ord") "Ord") [ toTsType a ]

modules :: Array (String /\ Array TS.Declaration)
modules =
  [ "Data.IxGraph" /\
      join
        [ typeDef "IxGraph" (Proxy :: _ (Data.IxGraph.IxGraph A B C))
        , value "neighborNodes"
            [ _Ord ORD ]
            (Data.IxGraph.neighborNodes :: ORD -> _ ORD B C -> _ (_ (ErrNeighborNodes ORD ())) _)
        ]
  , "CirclesPink.Data.Address" /\
      join
        [ typeDef "Address" (Proxy :: _ CirclesPink.Data.Address.Address)
        , instanceDef "ordAddress" (_Ord (Proxy :: _ CirclesPink.Data.Address.Address))
        ]
  , "CirclesPink.Data.TrustNode" /\
      join
        [ typeDef "TrustNode" (Proxy :: _ CirclesPink.Data.TrustNode.TrustNode)
        , value "unwrap" [] CirclesPink.Data.TrustNode.unwrap
        , instanceDef "ordTrustNode" (_Ord (Proxy :: _ CirclesPink.Data.TrustNode.TrustNode))
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

--------------------------------------------------------------------------------

newtype IxGraph id e n = IxGraph (Data.IxGraph.IxGraph id e n)

instance toTsTypeDef_IxGraph :: ToTsDef (IxGraph id e n) where
  toTsDef _ = pure $ TS.typeDef (TS.name "IxGraph") []
    $ TS.opaque (TS.qualName "Data_IxGraph" "IxGraph")
    $ TS.Name <$> [ "Id", "E", "N" ]

instance toTsType_IxGraph :: (ToTsType id, ToTsType e, ToTsType n) => ToTsType (IxGraph id e n) where
  toTsType _ = TS.mkType (TS.qualName "Data_IxGraph" "IxGraph") $
    [ toTsType (Proxy :: _ id)
    , toTsType (Proxy :: _ e)
    , toTsType (Proxy :: _ n)
    ]

--------------------------------------------------------------------------------

unsafeReplace :: forall a b. UnsafeReplace a b => a -> b
unsafeReplace = unsafeCoerce

class UnsafeReplace :: forall k1 k2. k1 -> k2 -> Constraint
class UnsafeReplace a b | a -> b

instance replaceIxGraph :: (UnsafeReplace a a', UnsafeReplace b b', UnsafeReplace c c') => UnsafeReplace (Data.IxGraph.IxGraph a b c) (IxGraph a' b' c')

else instance replaceFn :: (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Function a b) (Function a' b')

else instance replaceProxy :: (UnsafeReplace a a') => UnsafeReplace (Proxy a) (Proxy a')

else instance replace :: UnsafeReplace a a

--------------------------------------------------------------------------------

typeDef :: forall t154 t155. ToTsDef t154 => UnsafeReplace t155 (Proxy t154) => String -> t155 -> Array TS.Declaration
typeDef s x = PT.typeDef s (unsafeReplace x)

value :: forall t88 t89. ToTsType t88 => UnsafeReplace t89 t88 => String -> Array TS.Type -> t89 -> Array TS.Declaration
value s xs x = PT.value s xs (unsafeReplace x)
