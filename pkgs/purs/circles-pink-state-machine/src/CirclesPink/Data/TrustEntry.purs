module CirclesPink.Data.TrustEntry
  ( TrustEntry(..)
  , isCandidate
  , isConfirmed
  , matchTrustEntry
  , trustEntryToTrust
  ) where

import Prelude

import CirclesPink.Data.Trust (Trust)
import CirclesPink.Data.UserIdent (getAddress)
import Data.Generic.Rep (class Generic)
import Data.IxGraph (class Indexed)
import Data.Show.Generic (genericShow)
import CirclesPink.Data.Address (Address)

data TrustEntry = TrustCandidate Trust | TrustConfirmed Trust

derive instance ordTrustEntry :: Ord TrustEntry

derive instance eqTrustEntry :: Eq TrustEntry

derive instance genericTrustEntry :: Generic TrustEntry _

instance showTrustEntry :: Show TrustEntry where
  show = genericShow

instance indexedTrustEntry :: Indexed Address TrustEntry where
  getIndex (TrustCandidate { user }) = getAddress user
  getIndex (TrustConfirmed { user }) = getAddress user

isConfirmed :: TrustEntry -> Boolean
isConfirmed te = matchTrustEntry (\_ -> true) (\_ -> false) te

isCandidate :: TrustEntry -> Boolean
isCandidate te = matchTrustEntry (\_ -> false) (\_ -> true) te

matchTrustEntry :: forall z. (Trust -> z) -> (Trust -> z) -> TrustEntry -> z
matchTrustEntry confirmed candidate te = case te of
  TrustConfirmed t -> confirmed t
  TrustCandidate t -> candidate t

trustEntryToTrust :: TrustEntry -> Trust
trustEntryToTrust = matchTrustEntry identity identity
