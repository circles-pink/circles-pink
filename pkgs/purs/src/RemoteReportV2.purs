module RemoteReportV2
  ( RemoteReport
  , RemoteReport_
  , getData
  , getData_
  , getTimestamp
  , notAsked
  , setFailure
  , setLoading
  , setSuccess
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Variant as V
import RemoteData (RemoteData)
import RemoteData as RD

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type NotAsked :: forall k. (k -> Type) -> k -> Type
type NotAsked f a =
  { initialData :: f a
  }

type Loading :: forall k. (k -> Type) -> k -> Type
type Loading f a =
  { requestedAt :: Instant
  , retry :: Int
  , previousData :: f a
  }

type Failure :: forall k. (k -> Type) -> Type -> k -> Type
type Failure f e a =
  { requestedAt :: Instant
  , error :: e
  , retry :: Int
  , previousData :: f a
  }

type Success f a =
  { requestedAt :: Instant
  , data_ :: a
  , retry :: Int
  , previousData :: f a
  }

data RemoteReport f e a = RemoteReport
  (RemoteData (NotAsked f a) (Loading f a) (Failure f e a) (Success f a))

type RemoteReport_ e a = RemoteReport Identity e a

--------------------------------------------------------------------------------
-- Destructors
--------------------------------------------------------------------------------

getData :: forall f e a. Applicative f => RemoteReport f e a -> f a
getData = match
  { notAsked: \{ initialData } -> initialData
  , loading: \{ previousData } -> previousData
  , failure: \{ previousData } -> previousData
  , success: \{ data_ } -> pure data_
  }

getData_ :: forall e a. RemoteReport_ e a -> a
getData_ = getData >>> unwrap

getTimestamp :: forall f e a. RemoteReport f e a -> Instant
getTimestamp = match
  { notAsked: \_ -> bottom
  , loading: \{ requestedAt } -> requestedAt
  , failure: \{ requestedAt } -> requestedAt
  , success: \{ requestedAt } -> requestedAt
  }

--------------------------------------------------------------------------------

type MatchCases f e a z =
  { notAsked :: NotAsked f a -> z
  , loading :: Loading f a -> z
  , failure :: Failure f e a -> z
  , success :: Success f a -> z
  }

match :: forall f e a z. MatchCases f e a z -> RemoteReport f e a -> z
match m (RemoteReport x) = V.match m (unwrap x)

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
notAsked :: forall f e a. f a -> RemoteReport f e a
notAsked initialData = RemoteReport (RD._notAsked { initialData })

--------------------------------------------------------------------------------

type Opts_setLoading =
  { retry :: Int
  , requestedAt :: Instant
  }

setLoading :: forall f e a. Applicative f => Opts_setLoading -> RemoteReport f e a -> Maybe (RemoteReport f e a)
setLoading { retry, requestedAt } x = do
  when (requestedAt < getTimestamp x) bottom
  pure $ RemoteReport $
    RD._loading { requestedAt, retry, previousData: getData x }

--------------------------------------------------------------------------------

type Opts_setFailure e =
  { error :: e
  , retry :: Int
  , requestedAt :: Instant
  }

setFailure :: forall f e a. Applicative f => Opts_setFailure e -> RemoteReport f e a -> Maybe (RemoteReport f e a)
setFailure { error, retry, requestedAt } x = do
  when (requestedAt < getTimestamp x) bottom
  pure $ RemoteReport $
    RD._failure { error, requestedAt, retry, previousData: getData x }

--------------------------------------------------------------------------------

type Opts_setSuccess a =
  { data_ :: a
  , retry :: Int
  , requestedAt :: Instant
  }

setSuccess :: forall f e a. Applicative f => Opts_setSuccess a -> RemoteReport f e a -> Maybe (RemoteReport f e a)
setSuccess { data_, retry, requestedAt } x = do
  when (requestedAt < getTimestamp x) bottom
  pure $ RemoteReport $ RD._success { data_, requestedAt, retry, previousData: getData x }

--------------------------------------------------------------------------------

