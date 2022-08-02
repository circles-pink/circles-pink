module RemoteData
  ( RemoteData(..)
  , _failure
  , _loading
  , _notAsked
  , _success
  , isFailure
  , isLoading
  , isNotAsked
  , isSuccess
  , onSuccess
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, default, inj, onMatch)
import PursTsGen (class ToTsDef, class ToTsType, genericToTsDef, toTsType)
import PursTsGen.Class.ToPursType (class ToPursType, toPursType)
import PursTsGen.Data.ABC (A, B, C, D)
import PursTsGen.Lang.PureScript.Type as PS
import PursTsGen.Lang.TypeScript.DSL as TS
import Type.Proxy (Proxy(..))

newtype RemoteData n l e a = RemoteData
  ( Variant
      ( notAsked :: n
      , loading :: l
      , failure :: e
      , success :: a
      )
  )

derive instance genericRemoteData :: Generic (RemoteData n l e a) _
derive instance newtypeRemoteData :: Newtype (RemoteData n l e a) _
derive newtype instance showRemoteData :: (Show n, Show l, Show e, Show a) => Show (RemoteData n l e a)

instance toTsDefRemoteData :: ToTsDef (RemoteData A B C D) where
  toTsDef = genericToTsDef "RemoteData"

instance toTsTypeRemoteData ::
  ( ToTsType n
  , ToTsType l
  , ToTsType e
  , ToTsType a
  ) =>
  ToTsType (RemoteData n l e a) where
  toTsType _ = TS.mkType (TS.qualName "RemoteData" "RemoteData") $
    [ toTsType (Proxy :: _ n)
    , toTsType (Proxy :: _ l)
    , toTsType (Proxy :: _ e)
    , toTsType (Proxy :: _ a)
    ]

instance toPursTypeRemoteData ::
  ( ToPursType n
  , ToPursType l
  , ToPursType e
  , ToPursType a
  ) =>
  ToPursType (RemoteData n l e a) where
  toPursType _ = PS.mkType (PS.qualName "RemoteData" "RemoteData")
    [ toPursType (Proxy :: _ n)
    , toPursType (Proxy :: _ l)
    , toPursType (Proxy :: _ e)
    , toPursType (Proxy :: _ a)
    ]

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
