module CirclesPink.GenerateTSD.Replace where

import CirclesPink.GenerateTSD.Wrappers as W
import Data.DateTime.Instant as Data.DateTime.Instant
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument, Constructor, Product, Sum)
import Data.IxGraph as Data.IxGraph
import Data.Newtype (class Newtype)
import Data.Pair as Data.Pair
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Nil, Cons)
import PursTsGen (class GenToTsDefSum, class ToTsDef, PursType(..), defaultToPursType, defaultToTsType)
import PursTsGen as PT
import PursTsGen.Class.ToPursType (class ToPursType)
import PursTsGen.Class.ToTsDef (genericToTsDef')
import PursTsGen.Class.ToTsType (class ToTsType)
import PursTsGen.Lang.TypeScript.DSL as TS
import RemoteData (RemoteData)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

unsafeReplace :: forall a b. UnsafeReplace a b => a -> b
unsafeReplace = unsafeCoerce

class UnsafeReplace :: forall k1 k2. k1 -> k2 -> Constraint
class UnsafeReplace a b | a -> b

instance replaceIxGraph ::
  ( UnsafeReplace a a'
  , UnsafeReplace b b'
  , UnsafeReplace c c'
  ) =>
  UnsafeReplace (Data.IxGraph.IxGraph a b c) (W.IxGraph a' b' c')

else instance replaceNeighborConnectivity ::
  ( UnsafeReplace a a'
  ) =>
  UnsafeReplace (Data.IxGraph.NeighborConnectivity a) (W.NeighborConnectivity a')

else instance replacePair ::
  ( UnsafeReplace a a'
  ) =>
  UnsafeReplace (Data.Pair.Pair a) (Pair a')

else instance replaceInstant ::
  UnsafeReplace Data.DateTime.Instant.Instant W.Instant

else instance replaceFn :: (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Function a b) (Function a' b')

else instance replaceTuple :: (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Tuple a b) (Tuple a' b')

else instance replaceEither :: (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Either a b) (Either a' b')

else instance replaceArray :: (UnsafeReplace a a') => UnsafeReplace (Array a) (Array a')

else instance replaceProxy :: (UnsafeReplace a a') => UnsafeReplace (Proxy a) (Proxy a')

else instance replaceRemoteData ::
  ( UnsafeReplace a a'
  , UnsafeReplace b b'
  , UnsafeReplace c c'
  , UnsafeReplace d d'
  ) =>
  UnsafeReplace (RemoteData a b c d) (RemoteData a' b' c' d')

else instance replaceRecord :: (RowToList a rl, GenRecord rl a') => UnsafeReplace (Record a) (Record a')

else instance replaceSum :: (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Sum a b) (Sum a' b')

else instance replaceConstructor :: (UnsafeReplace a a') => UnsafeReplace (Constructor s a) (Constructor s a')

else instance replaceProduct :: (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Product a b) (Product a' b')

else instance replaceArgument :: (UnsafeReplace a a') => UnsafeReplace (Argument a) (Argument a')

else instance replaceVariant :: (RowToList a rl, GenRecord rl a') => UnsafeReplace (Variant a) (Variant a')

else instance replace :: UnsafeReplace a a

--------------------------------------------------------------------------------

class GenRecord :: forall k1 k2. k1 -> k2 -> Constraint
class GenRecord rl r | rl -> r

instance genRecordNil :: GenRecord Nil ()

instance genRecordCons ::
  ( GenRecord rl r'
  , Cons s t' r' r''
  , UnsafeReplace t t'
  ) =>
  GenRecord (Cons s t rl) r''

--------------------------------------------------------------------------------

typeDef :: forall t154 t155. ToTsDef t154 => UnsafeReplace t155 (Proxy t154) => String -> t155 -> Array TS.Declaration
typeDef s x = PT.typeDef s (unsafeReplace x)

value :: forall t88 t89. ToTsType t88 => UnsafeReplace t89 t88 => String -> Array TS.Type -> t89 -> Array TS.Declaration
value s xs x = PT.value s xs (unsafeReplace x)

typeAlias :: forall t119 t120. ToTsType t119 => UnsafeReplace t120 t119 => String -> t120 -> Array TS.Declaration
typeAlias n x = PT.typeAlias n (unsafeReplace x)

genericToTsDef
  :: forall a rep rep'
   . UnsafeReplace rep rep'
  => ToPursType a
  => Generic a rep
  => GenToTsDefSum rep'
  => String
  -> Proxy a
  -> Array TS.Declaration
genericToTsDef s p = genericToTsDef' s p (Proxy :: _ rep')

--------------------------------------------------------------------------------

infixr 6 type Sum as :+:
infixl 7 type Product as :*:

--------------------------------------------------------------------------------

newtype Pair a = Pair (Data.Pair.Pair a)


ptPair :: PursType
ptPair = PursType "Data_Pair" "Pair"


derive instance newtypePair :: Newtype (Pair a) _

instance g ::
  UnsafeReplace a a' =>
  Generic (Pair a)
    (Constructor "Pair" (Argument a' :*: Argument a'))
  where
  from = undefined
  to = undefined

instance toTsType_Pair :: (ToTsType a) => ToTsType (Pair a) where
  toTsType _ = defaultToTsType ptPair []

instance toTsDef_Pair :: (ToPursType a, ToTsType a) => ToTsDef (Pair a) where
  toTsDef = genericToTsDef "Pair"

instance toPursType_Pair :: (ToPursType a) => ToPursType (Pair a) where
  toPursType _ = defaultToPursType ptPair []