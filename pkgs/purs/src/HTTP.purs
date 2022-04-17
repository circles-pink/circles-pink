module HTTP
  ( NetworkError(..)
  , Req
  , ReqFn
  , Res
  , _errUnknown
  , addLogging
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (Json, stringify)
import Data.Either (Either)
import Data.HTTP.Method (Method)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type Req
  = { url :: String, method :: Method, body :: Json }

type Res
  = { status :: Int, body :: Json }

type NetworkError r
  = ( errNetwork :: Unit | r )

_errUnknown :: forall r. Variant (NetworkError + r)
_errUnknown = inj (Proxy :: _ "errNetwork") unit

type ReqFn r
  = Req -> ExceptV (NetworkError + r) Aff Res

addLogging :: forall r. ReqFn r -> ReqFn r
addLogging reqFn req = do
  log ("HTTP REQUEST: " <> show req.method <> " " <> show req.url <> " " <> (stringify req.body))
  res <- reqFn req
  log ("HTTP RESPONSE: " <> show res.status <> " " <> (stringify req.body))
  pure res

-- 1234567891234567
