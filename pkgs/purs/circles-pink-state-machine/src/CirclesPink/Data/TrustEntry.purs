module CirclesPink.Data.TrustEntry
  ( TrustEntry(..)
  , isCandidate
  , isConfirmed
  , matchTrustEntry
  , trustEntryToTrust
  )
  where

import Prelude

import CirclesPink.Data.Trust (Trust)
import Convertable (convert)
import Data.Either (either)
import Data.IxGraph (class Indexed)
import Network.Ethereum.Core.Signatures as W3

data TrustEntry = TrustCandidate Trust | TrustConfirmed Trust

derive instance ordTrustEntry :: Ord TrustEntry

derive instance eqTrustEntry :: Eq TrustEntry

instance indexedTrustEntry :: Indexed W3.Address TrustEntry  where
  getIndex (TrustCandidate { user }) = either identity (_.safeAddress >>> convert) user
  getIndex (TrustConfirmed { user }) = either identity (_.safeAddress >>> convert) user

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
