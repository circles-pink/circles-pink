module RemoteReport
  ( RemoteReport
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe)
import RemoteData (RemoteData)
import URI.Scheme.Common (acap)

type RemoteReport e a = RemoteData
  Unit
  { timestamp :: Instant, retry :: Int, previousData :: Maybe a }
  { error :: e, timestamp :: Instant, retry :: Int }
  { data :: a, timestamp :: Instant, retry :: Int }
