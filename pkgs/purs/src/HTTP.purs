module HTTP
  ( NetworkError(..)
  , Req
  , ReqFn
  , Res
  , _errUnknown
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.HTTP.Method (Method)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
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
