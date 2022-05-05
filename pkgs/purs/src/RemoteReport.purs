module RemoteReport
  ( RemoteReport
  ) where

import Prelude
import Data.DateTime.Instant (Instant)
import RemoteData (RemoteData)

type RemoteReport e a
  = RemoteData Unit
      { timestamp :: Instant, retry :: Int }
      { error :: e, timestamp :: Instant, retry :: Int }
      { data :: a, timestamp :: Instant, retry :: Int }
