module HTTP
  ( ErrNetwork
  , ErrParseJson
  , ErrReqFn
  , Req
  , ReqFn
  , Res
  , _errNetwork
  , _errParseJson
  , addLogging
  ) where

import Prelude
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (Json, stringify)
import Data.HTTP.Method (Method)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type Req = { url :: String, method :: Method, body :: Json }

type Res = { status :: Int, body :: Json }

--------------------------------------------------------------------------------
type ErrNetwork r = (errNetwork :: Req | r)

_errNetwork :: forall r. Req -> Variant (ErrNetwork + r)
_errNetwork = inj (Proxy :: _ "errNetwork")

type ErrParseJson r = (errParseJson :: Unit | r)

_errParseJson :: forall r. Variant (ErrParseJson + r)
_errParseJson = inj (Proxy :: _ "errParseJson") unit

--------------------------------------------------------------------------------
type ErrReqFn r = ErrNetwork + ErrParseJson + r

type ReqFn r = Req -> ExceptV (ErrReqFn + r) Aff Res

--------------------------------------------------------------------------------
addLogging :: forall r. ReqFn r -> ReqFn r
addLogging reqFn req = do
  log ("HTTP REQUEST: " <> show req.method <> " " <> show req.url <> " " <> (stringify req.body))
  res <- reqFn req
  log ("HTTP RESPONSE: " <> show res.status <> " " <> (stringify req.body))
  pure res
