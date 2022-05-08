module RemoteData
  ( RemoteData(..)
  , _failure
  , _loading
  , _notAsked
  , _success
  , isSuccess
  ) where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, default, inj, onMatch)
import Type.Proxy (Proxy(..))

newtype RemoteData n l e a
  = RemoteData
  ( Variant
      ( notAsked :: n
      , loading :: l
      , failure :: e
      , success :: a
      )
  )

derive instance newtypeRemoteData :: Newtype (RemoteData n l e a) _

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
isSuccess :: forall n l e a. RemoteData n l e a -> Boolean
isSuccess =
  unwrap
    >>> ( default false
          # onMatch
              { success: const true }
      )
