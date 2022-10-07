module CirclesPink.Data.TrustNode
  ( TrustNode(..)
  , _root
  , _userIdent
  , getAddress
  , initTrustNode
  )
  where

import CirclesPink.Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.User (User(..))
import CirclesPink.Data.UserIdent (UserIdent(..))
import Data.IxGraph (class Indexed)
import Data.IxGraph as IxGraph
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import PursTsGen.Lang.PureScript.Type as PS
import Type.Proxy (Proxy(..))

newtype TrustNode = TrustNode
  { userIdent :: UserIdent
  , isLoading :: Boolean
  , root :: Address
  }

moduleName :: String
moduleName = "CirclesPink.Data.TrustNode"

getAddress :: TrustNode -> Address
getAddress = IxGraph.getIndex

initTrustNode :: Address -> UserIdent -> TrustNode
initTrustNode root userIdent' = TrustNode
  { userIdent: userIdent'
  , isLoading: false
  , root
  }

_userIdent :: Lens' TrustNode UserIdent
_userIdent = _Newtype <<< prop (Proxy :: _ "userIdent")

_root :: Lens' TrustNode Address
_root = _Newtype <<< prop (Proxy :: _ "root")

derive newtype instance Show TrustNode
derive newtype instance Eq TrustNode
derive newtype instance Ord TrustNode

derive instance Newtype TrustNode _

instance Indexed Address TrustNode where
  getIndex (TrustNode { userIdent: UserIdent (Left x) }) = x
  getIndex (TrustNode { userIdent: UserIdent (Right (User { safeAddress })) }) = safeAddress

instance ToPursNominal TrustNode where
  toPursNominal _ = PursNominal moduleName "TrustNode"

instance ToTsDef TrustNode where
  toTsDef = newtypeToTsDef []

instance ToTsType TrustNode where
  toTsType = typeRefToTsType' []

instance ToPursType TrustNode where
  toPursType _ = PS.var $ PS.Name "TODO"
