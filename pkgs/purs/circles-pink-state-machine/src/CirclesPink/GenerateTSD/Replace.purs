module CirclesPink.GenerateTSD.Replace where

import Prelude

import CirclesPink.GenerateTSD.Wrappers as W
import Data.DateTime.Instant as Data.DateTime.Instant
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument, Constructor, Product, Sum)
import Data.IxGraph as Data.IxGraph
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Pair as Data.Pair
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Network.Ethereum.Core.Signatures as Network.Ethereum.Core.Signatures
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Nil, Cons)
import PursTsGen (class GenToTsDefSum, class ToTsDef, class ToTsType, PursType(..), defaultToPursType, defaultToTsDef, defaultToTsType, toPursType, toTsType)
import PursTsGen as PT
import PursTsGen.Class.ToPursType (class ToPursType)
import PursTsGen.Class.ToTsDef (genericToTsDef')
import PursTsGen.Class.ToTsType (class ToTsType, toTsType) as P
import PursTsGen.Lang.TypeScript.DSL as TS
import RemoteData as RemoteData
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)
import Data.Argonaut as Data.Argonaut
import Data.BN as Data.BN

--------------------------------------------------------------------------------

unsafeReplace :: forall a b. UnsafeReplace a b => a -> b
unsafeReplace = unsafeCoerce

class UnsafeReplace :: forall k1 k2. k1 -> k2 -> Constraint
class UnsafeReplace a b | a -> b

instance
  ( UnsafeReplace a a'
  , UnsafeReplace b b'
  , UnsafeReplace c c'
  ) =>
  UnsafeReplace (Data.IxGraph.IxGraph a b c) (W.IxGraph a' b' c')

else instance
  ( UnsafeReplace a a'
  ) =>
  UnsafeReplace (Data.IxGraph.NeighborConnectivity a) (W.NeighborConnectivity a')

else instance
  ( UnsafeReplace a a'
  ) =>
  UnsafeReplace (Data.Pair.Pair a) (Pair a')

else instance UnsafeReplace Data.Argonaut.JsonDecodeError JsonDecodeError

else instance UnsafeReplace Data.BN.BN BN

else instance UnsafeReplace Network.Ethereum.Core.Signatures.Address Address

else instance
  UnsafeReplace Data.DateTime.Instant.Instant W.Instant

else instance (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Function a b) (Function a' b')

else instance (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Tuple a b) (Tuple a' b')

else instance (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Either a b) (Either a' b')

else instance (UnsafeReplace a a') => UnsafeReplace (Maybe a) (Maybe a')

else instance (UnsafeReplace a a') => UnsafeReplace (Array a) (Array a')

else instance (UnsafeReplace a a') => UnsafeReplace (Proxy a) (Proxy a')

else instance
  ( UnsafeReplace a a'
  , UnsafeReplace b b'
  , UnsafeReplace c c'
  , UnsafeReplace d d'
  ) =>
  UnsafeReplace (RemoteData.RemoteData a b c d) (RemoteData a' b' c' d')

else instance (RowToList a rl, GenRecord rl a') => UnsafeReplace (Record a) (Record a')

else instance (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Sum a b) (Sum a' b')

else instance (UnsafeReplace a a') => UnsafeReplace (Constructor s a) (Constructor s a')

else instance (UnsafeReplace a a', UnsafeReplace b b') => UnsafeReplace (Product a b) (Product a' b')

else instance (UnsafeReplace a a') => UnsafeReplace (Argument a) (Argument a')

else instance (RowToList a rl, GenRecord rl a') => UnsafeReplace (Variant a) (Variant a')

else instance UnsafeReplace a a

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

value :: forall t88 t89. P.ToTsType t88 => UnsafeReplace t89 t88 => String -> Array TS.Type -> t89 -> Array TS.Declaration
value s xs x = PT.value s xs (unsafeReplace x)

typeAlias :: forall t119 t120. P.ToTsType t119 => UnsafeReplace t120 t119 => String -> t120 -> Array TS.Declaration
typeAlias n x = PT.typeAlias n (unsafeReplace x)

-- toTsType :: forall a a'. UnsafeReplace a a' => ToTsType a => a -> TS.Type
-- toTsType x = P.toTsType $ unsafeReplace x

-- class (UnsafeReplace a a', P.ToTsType a') <= ToTsType a

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

newtype BN = BN Data.BN.BN

ptBN :: PursType
ptBN = PursType "Data_BN" "BN"

derive instance Newtype BN _

instance P.ToTsType BN where
  toTsType _ = defaultToTsType ptBN []

instance ToTsDef BN where
  toTsDef _ = defaultToTsDef ptBN []

instance ToPursType BN where
  toPursType _ = defaultToPursType ptBN []


--------------------------------------------------------------------------------

newtype JsonDecodeError = JsonDecodeError Data.Argonaut.JsonDecodeError

ptJsonDecodeError :: PursType
ptJsonDecodeError = PursType "Data_Argonaut" "JsonDecodeError"

derive instance Newtype JsonDecodeError _

instance P.ToTsType JsonDecodeError where
  toTsType _ = defaultToTsType ptJsonDecodeError []

instance ToTsDef JsonDecodeError where
  toTsDef _ = defaultToTsDef ptJsonDecodeError []

instance ToPursType JsonDecodeError where
  toPursType _ = defaultToPursType ptJsonDecodeError []

--------------------------------------------------------------------------------

newtype Pair a = Pair (Data.Pair.Pair a)

ptPair :: PursType
ptPair = PursType "Data_Pair" "Pair"

derive instance Newtype (Pair a) _

instance g ::
  UnsafeReplace a a' =>
  Generic (Pair a)
    (Constructor "Pair" (Argument a' :*: Argument a'))
  where
  from = undefined
  to = undefined

instance (P.ToTsType a) => P.ToTsType (Pair a) where
  toTsType _ = defaultToTsType ptPair [ P.toTsType (Proxy :: _ a) ]

instance (ToPursType a, P.ToTsType a) => ToTsDef (Pair a) where
  toTsDef = genericToTsDef "Pair"

instance (ToPursType a) => ToPursType (Pair a) where
  toPursType _ = defaultToPursType ptPair []

--------------------------------------------------------------------------------

newtype Address = Address Network.Ethereum.Core.Signatures.Address

ptAddress :: PursType
ptAddress = PursType "Network_Ethereum_Core_Signatures" "Address"

derive instance newtypeAddress :: Newtype Address _

instance P.ToTsType Address where
  toTsType _ = defaultToTsType ptAddress []

instance ToTsDef Address where
  toTsDef _ = defaultToTsDef ptAddress []

instance ToPursType Address where
  toPursType _ = defaultToPursType ptAddress []

--------------------------------------------------------------------------------

newtype RemoteData a b c d = RemoteData (RemoteData.RemoteData a b c d)

ptRemoteData :: PursType
ptRemoteData = PursType "RemoteData" "RemoteData"

derive instance newtypeRemoteData :: Newtype (RemoteData a b c d) _

instance toTsType_RemoteData ::
  ( ToTsType a
  , ToTsType b
  , ToTsType c
  , ToTsType d
  ) =>
  ToTsType (RemoteData a b c d) where
  toTsType _ = defaultToTsType ptRemoteData
    [ toTsType (Proxy :: _ a)
    , toTsType (Proxy :: _ b)
    , toTsType (Proxy :: _ c)
    , toTsType (Proxy :: _ d)
    ]

instance toTsDef_RemoteData :: ToTsDef (RemoteData a b c d) where
  toTsDef _ = defaultToTsDef ptRemoteData $ TS.name <$> [ "A", "B", "C", "D" ]

instance toPursType_RemoteData ::
  ( ToPursType a
  , ToPursType b
  , ToPursType c
  , ToPursType d
  ) =>
  ToPursType (RemoteData a b c d) where
  toPursType _ = defaultToPursType ptRemoteData
    [ toPursType (Proxy :: _ a)
    , toPursType (Proxy :: _ b)
    , toPursType (Proxy :: _ c)
    , toPursType (Proxy :: _ d)
    ]