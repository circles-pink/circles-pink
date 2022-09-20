module CirclesPink.Data.TrustNode where

import CirclesPink.Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.User (User(..))
import CirclesPink.Data.UserIdent (UserIdent(..))
import Data.IxGraph (class Indexed)
import Data.IxGraph as IxGraph 
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype as NT
import PursTsGen (class ToTsDef, class ToTsType)
import PursTsGen.Lang.TypeScript.DSL as TS
import Type.Proxy (Proxy(..))

type TrustNode' = { userIdent :: UserIdent }

newtype TrustNode = TrustNode TrustNode'

derive instance Newtype TrustNode _
derive newtype instance Show TrustNode
derive newtype instance Eq TrustNode
derive newtype instance Ord TrustNode

instance ToTsDef TrustNode where
  toTsDef _ = pure $ TS.typeDef (TS.name "TrustNode") []
    $ TS.opaque (TS.qualName "CirclesPink_Data_TrustNode" "TrustNode") $ TS.name <$> []

instance ToTsType TrustNode where
  toTsType _ = TS.mkType_ $ TS.qualName "CirclesPink_Data_TrustNode" "TrustNode"

unwrap :: TrustNode -> TrustNode'
unwrap = NT.unwrap

instance Indexed Address TrustNode where
  getIndex (TrustNode { userIdent: UserIdent (Left x) }) = x
  getIndex (TrustNode { userIdent: UserIdent (Right (User { safeAddress })) }) = safeAddress

getAddress :: TrustNode -> Address
getAddress = IxGraph.getIndex

initTrustNode :: UserIdent -> TrustNode
initTrustNode userIdent' = TrustNode { userIdent: userIdent' }

userIdent :: Lens' TrustNode UserIdent
userIdent = _Newtype <<< prop (Proxy :: _ "userIdent")
