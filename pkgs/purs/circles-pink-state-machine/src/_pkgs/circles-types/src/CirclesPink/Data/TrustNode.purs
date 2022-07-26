module CirclesPink.Data.TrustNode where

import CirclesPink.Prelude

import CirclesPink.Data.Address (Address)
import CirclesPink.Data.UserIdent (UserIdent(..))
import Data.IxGraph (class Indexed)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import FpTs.Class (class FpTs)
import Type.Proxy (Proxy(..))
import Data.Newtype as NT

type TrustNode' = { userIdent :: UserIdent }

newtype TrustNode = TrustNode TrustNode'

derive instance newtypeTrusNode :: Newtype TrustNode _

derive newtype instance showTrustNode :: Show TrustNode

derive newtype instance eqTrustNode :: Eq TrustNode

derive newtype instance ordTrustNode :: Ord TrustNode

unwrap :: TrustNode -> TrustNode'
unwrap = NT.unwrap

instance fpTsTrustNode :: FpTs TrustNode TrustNode where
  fromFpTs = identity
  toFpTs = identity

instance indexedUserIdent :: Indexed Address TrustNode where
  getIndex (TrustNode { userIdent: UserIdent (Left x) }) = x
  getIndex (TrustNode { userIdent: UserIdent (Right { safeAddress }) }) = safeAddress

initTrustNode :: UserIdent -> TrustNode
initTrustNode userIdent' = TrustNode { userIdent: userIdent' }

userIdent :: Lens' TrustNode UserIdent
userIdent = _Newtype <<< prop (Proxy :: _ "userIdent")
