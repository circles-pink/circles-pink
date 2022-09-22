module RemoteReport
  ( RemoteReport
  , getData
  , getData'
  )
  where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Variant (case_, onMatch)
import RemoteData (RemoteData(..))

type RemoteReport e a = RemoteData
  Unit
  { timestamp :: Instant, retry :: Int, previousData :: Maybe a }
  { error :: e, timestamp :: Instant, retry :: Int }
  { data :: a, previousData :: Maybe a, timestamp :: Instant, retry :: Int }

getData :: forall e a. a -> RemoteReport e a -> a
getData def (RemoteData v) =
  ( case_ # onMatch
      { notAsked: \_ -> def
      , loading: \{ previousData } -> fromMaybe def previousData
      , failure: \_ -> def
      , success: \{ data: data_ } -> data_
      }
  ) v

getData' :: forall e a. RemoteReport e a -> Maybe a
getData' (RemoteData v) =
  ( case_ # onMatch
      { notAsked: \_ -> Nothing
      , loading: \{ previousData } -> previousData
      , failure: \_ -> Nothing
      , success: \{ data: data_ } -> Just data_
      }
  ) v
