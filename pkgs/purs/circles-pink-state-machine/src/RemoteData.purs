module RemoteData
  ( RemoteData'
  , RemoteData(..)
  , _failure
  , _loading
  , _notAsked
  , _success
  , isFailure
  , isLoading
  , isNotAsked
  , isSuccess
  , onSuccess
  , unRemoteData
  , unwrap
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Data.Newtype as NT
import Data.Variant (Variant, case_, default, inj, onMatch)
import Type.Proxy (Proxy(..))

type RemoteData' n l e a = Variant
  ( notAsked :: n
  , loading :: l
  , failure :: e
  , success :: a
  )

newtype RemoteData n l e a = RemoteData (RemoteData' n l e a)

unRemoteData
  :: forall n l e a z
   . { onNotAsked :: n -> z
     , onLoading :: l -> z
     , onFailure :: e -> z
     , onSuccess :: a -> z
     }
  -> RemoteData n l e a
  -> z
unRemoteData c (RemoteData rd) =
  rd #
    ( case_ # onMatch
        { notAsked: c.onNotAsked
        , loading: c.onLoading
        , failure: c.onFailure
        , success: c.onSuccess
        }
    )

unwrap :: forall n l e a. RemoteData n l e a -> RemoteData' n l e a
unwrap = NT.unwrap

derive instance Newtype (RemoteData n l e a) _
derive newtype instance (Show n, Show l, Show e, Show a) => Show (RemoteData n l e a)

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
_notAsked :: forall n l e a. n -> RemoteData n l e a
_notAsked = RemoteData <<< inj (Proxy :: _ "notAsked")

_loading :: forall n l e a. l -> RemoteData n l e a
_loading = RemoteData <<< inj (Proxy :: _ "loading")

_failure :: forall n l e a. e -> RemoteData n l e a
_failure = RemoteData <<< inj (Proxy :: _ "failure")

_success :: forall n l e a. a -> RemoteData n l e a
_success = RemoteData <<< inj (Proxy :: _ "success")

--------------------------------------------------------------------------------
-- Destructors
--------------------------------------------------------------------------------
isNotAsked :: forall n l e a. RemoteData n l e a -> Boolean
isNotAsked = unwrap >>> (default false # onMatch { notAsked: const true })

isLoading :: forall n l e a. RemoteData n l e a -> Boolean
isLoading = unwrap >>> (default false # onMatch { loading: const true })

isFailure :: forall n l e a. RemoteData n l e a -> Boolean
isFailure = unwrap >>> (default false # onMatch { failure: const true })

isSuccess :: forall n l e a. RemoteData n l e a -> Boolean
isSuccess = unwrap >>> (default false # onMatch { success: const true })

onSuccess :: forall n l e a b. b -> (a -> b) -> RemoteData n l e a -> b
onSuccess def f x =
  ( default def
      # onMatch { success: f }
  )
    (unwrap x)
