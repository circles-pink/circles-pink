module CirclesPink.GenerateTSD.Replace where

import Prelude

import CirclesPink.Data.PrivateKey.Type as CirclesPink.Data.PrivateKey.Type
import CirclesPink.Garden.EnvControlAff (EnvVars(..))
import CirclesPink.Garden.StateMachine (CirclesConfig(..))
import CirclesPink.Garden.TS (CirclesConfigEffect(..))
import CirclesPink.GenerateTSD.Wrappers as W
import CirclesPink.URI (URI)
import Data.Argonaut (Json, JsonDecodeError)
import Data.BN (BN)
import Data.DateTime.Instant (Instant)
import Data.Either (Either)
import Data.Generic.Rep (class Generic, Argument, Constructor, Product, Sum)
import Data.HTTP.Method (Method)
import Data.IxGraph as Data.IxGraph
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Pair (Pair)
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Foreign.Object (Object)
import Milkis.Impl (FetchImpl)
import Network.Ethereum.Core.Signatures as Network.Ethereum.Core.Signatures
import Network.Ethereum.Core.Signatures.Extra (ChecksumAddress)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Nil, Cons)
import PursTsGen (class GenToTsDefSum, class ToPursNominal, class ToTsDef, class ToTsType, PursNominal(..), PursType(..), defaultToPursType, defaultToPursType', opaqueToTsDef, opaqueToTsDef', typeRefToTsType, typeRefToTsType', toPursType, toTsType)
import PursTsGen as PT
import PursTsGen.Class.ToPursType (class ToPursType)
import PursTsGen.Class.ToTsDef (genericToTsDef')
import PursTsGen.Data.ABC (A)
import PursTsGen.Lang.PureScript.Type as PS
import PursTsGen.Lang.TypeScript.DSL as TS
import RemoteData as RemoteData
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

newtype Wrap a = Wrap a

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
  UnsafeReplace (Pair a) (Wrap (Pair a'))

else instance UnsafeReplace JsonDecodeError (Wrap JsonDecodeError)

else instance UnsafeReplace BN (Wrap BN)

else instance UnsafeReplace CirclesPink.Data.PrivateKey.Type.PrivateKey PrivateKey

else instance UnsafeReplace Network.Ethereum.Core.Signatures.Address Address

else instance UnsafeReplace Method (Wrap Method)

else instance UnsafeReplace EnvVars (Wrap EnvVars)

else instance UnsafeReplace URI (Wrap URI)

else instance UnsafeReplace ChecksumAddress (Wrap ChecksumAddress)

else instance UnsafeReplace FetchImpl (Wrap FetchImpl)

else instance UnsafeReplace CirclesConfigEffect (Wrap CirclesConfigEffect)

else instance UnsafeReplace Json (Wrap Json)

else instance UnsafeReplace Milliseconds (Wrap Milliseconds)

else instance UnsafeReplace a a' => UnsafeReplace (Object a) (Wrap (Object a'))

else instance UnsafeReplace Instant (Wrap Instant)

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

instance GenRecord Nil ()

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

-- toTsType :: forall a a'. UnsafeReplace a a' => ToTsType a => a -> TS.Type
-- toTsType x = ToTsType $ unsafeReplace x

-- class (UnsafeReplace a a', ToTsType a') <= ToTsType a

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

instance ToPursNominal (Wrap BN) where
  toPursNominal _ = PursNominal "Data.BN" "BN"

instance ToTsType (Wrap BN) where
  toTsType = typeRefToTsType' []

instance ToTsDef (Wrap BN) where
  toTsDef = opaqueToTsDef' []

instance ToPursType (Wrap BN) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap Json) where
  toPursNominal _ = PursNominal "Data.Argonaut" "Json"

instance ToTsType (Wrap Json) where
  toTsType = typeRefToTsType' []

instance ToTsDef (Wrap Json) where
  toTsDef = opaqueToTsDef' []

instance ToPursType (Wrap Json) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap Milliseconds) where
  toPursNominal _ = PursNominal "Data.Time.Duration" "Milliseconds"

instance ToTsType (Wrap Milliseconds) where
  toTsType = defaultToTsType' []

instance ToTsDef (Wrap Milliseconds) where
  toTsDef = defaultToTsDef' []

instance ToPursType (Wrap Milliseconds) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap URI) where
  toPursNominal _ = PursNominal "CirclesPink.URI" "URI"

instance ToTsType (Wrap URI) where
  toTsType = typeRefToTsType' []

instance ToTsDef (Wrap URI) where
  toTsDef = opaqueToTsDef' []

instance ToPursType (Wrap URI) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap ChecksumAddress) where
  toPursNominal _ = PursNominal "Network.Ethereum.Core.Signatures.Extra" "ChecksumAddress"

instance ToTsType (Wrap ChecksumAddress) where
  toTsType = typeRefToTsType' []

instance ToTsDef (Wrap ChecksumAddress) where
  toTsDef = opaqueToTsDef' []

instance ToPursType (Wrap ChecksumAddress) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToTsType (Wrap CirclesConfigEffect) where
  toTsType (Wrap (CirclesConfigEffect (CirclesConfig r))) = toTsType $ unsafeReplace r

instance ToPursType (Wrap CirclesConfigEffect) where
  toPursType _ = PS.var $ PS.Name "TODO"

--------------------------------------------------------------------------------

instance ToTsType (Wrap EnvVars) where
  toTsType (Wrap (EnvVars r)) = toTsType $ unsafeReplace r

instance ToPursType (Wrap EnvVars) where
  toPursType _ = PS.var $ PS.Name "TODO"

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap (Object a)) where
  toPursNominal _ = PursNominal "Foreign.Object" "Object"

instance (ToTsType a) => ToTsType (Wrap (Object a)) where
  toTsType = typeRefToTsType' [ toTsType (Proxy :: _ a) ]

instance ToTsDef (Wrap (Object A)) where
  toTsDef = opaqueToTsDef' [ TS.name "A" ]

instance (ToPursType a) => ToPursType (Wrap (Object a)) where
  toPursType = defaultToPursType' [ toPursType (Proxy :: _ a) ]

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap Method) where
  toPursNominal _ = PursNominal "Data.HTTP.Method" "Method"

instance ToTsType (Wrap Method) where
  toTsType = typeRefToTsType' []

instance ToTsDef (Wrap Method) where
  toTsDef = opaqueToTsDef' []

instance ToPursType (Wrap Method) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap FetchImpl) where
  toPursNominal _ = PursNominal "Milkis.Impl" "FetchImpl"

instance ToTsType (Wrap FetchImpl) where
  toTsType = typeRefToTsType' []

instance ToTsDef (Wrap FetchImpl) where
  toTsDef = opaqueToTsDef' []

instance ToPursType (Wrap FetchImpl) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap JsonDecodeError) where
  toPursNominal _ = PursNominal "Data_Argonaut" "JsonDecodeError"

instance ToTsType (Wrap JsonDecodeError) where
  toTsType = typeRefToTsType' []

instance ToTsDef (Wrap JsonDecodeError) where
  toTsDef = opaqueToTsDef' []

instance ToPursType (Wrap JsonDecodeError) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap Instant) where
  toPursNominal _ = PursNominal "Data.DateTime.Instant" "Instant"

instance ToTsType (Wrap Instant) where
  toTsType = typeRefToTsType' []

instance ToTsDef (Wrap Instant) where
  toTsDef = opaqueToTsDef' []

instance ToPursType (Wrap Instant) where
  toPursType = defaultToPursType' []

--------------------------------------------------------------------------------

instance ToPursNominal (Wrap (Pair a)) where
  toPursNominal _ = PursNominal "Data.Pair" "Pair"

instance (ToTsType a) => ToTsType (Wrap (Pair a)) where
  toTsType = typeRefToTsType'
    [ toTsType (Proxy :: _ a)
    ]

instance ToTsDef (Wrap (Pair A)) where
  toTsDef = opaqueToTsDef' [ TS.name "A" ]

instance (ToPursType a) => ToPursType (Wrap (Pair a)) where
  toPursType = defaultToPursType' [ toPursType (Proxy :: _ a) ]

--------------------------------------------------------------------------------

newtype Address = Address Network.Ethereum.Core.Signatures.Address

ptAddress :: PursType
ptAddress = PursType "Network_Ethereum_Core_Signatures" "Address"

derive instance Newtype Address _

instance ToTsType Address where
  toTsType _ = typeRefToTsType ptAddress []

instance ToTsDef Address where
  toTsDef _ = opaqueToTsDef ptAddress []

instance ToPursType Address where
  toPursType _ = defaultToPursType ptAddress []

--------------------------------------------------------------------------------

newtype PrivateKey = PrivateKey CirclesPink.Data.PrivateKey.Type.PrivateKey

ptPrivateKey :: PursType
ptPrivateKey = PursType "CirclesPink_Data_PrivateKey_Type" "PrivateKey"

derive instance Newtype PrivateKey _

instance ToTsType PrivateKey where
  toTsType _ = typeRefToTsType ptPrivateKey []

instance ToTsDef PrivateKey where
  toTsDef _ = opaqueToTsDef ptPrivateKey []

instance ToPursType PrivateKey where
  toPursType _ = defaultToPursType ptPrivateKey []

--------------------------------------------------------------------------------

newtype RemoteData a b c d = RemoteData (RemoteData.RemoteData a b c d)

ptRemoteData :: PursType
ptRemoteData = PursType "RemoteData" "RemoteData"

derive instance Newtype (RemoteData a b c d) _

instance toTsType_RemoteData ::
  ( ToTsType a
  , ToTsType b
  , ToTsType c
  , ToTsType d
  ) =>
  ToTsType (RemoteData a b c d) where
  toTsType _ = typeRefToTsType ptRemoteData
    [ toTsType (Proxy :: _ a)
    , toTsType (Proxy :: _ b)
    , toTsType (Proxy :: _ c)
    , toTsType (Proxy :: _ d)
    ]

instance ToTsDef (RemoteData a b c d) where
  toTsDef _ = opaqueToTsDef ptRemoteData $ TS.name <$> [ "A", "B", "C", "D" ]

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