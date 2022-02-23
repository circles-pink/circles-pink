module HTTP
  ( NetworkError(..)
  , Req
  , ReqFn
  , Res
  ) where

import Data.Argonaut (Json)
import Data.Either (Either)
import Data.HTTP.Method (Method)
import Effect.Aff (Aff)

type Req
  = { url :: String, method :: Method, body :: Json }

type Res
  = { status :: Int, body :: Json }

data NetworkError
  = ErrUnknown

type ReqFn
  = Req -> Aff (Either NetworkError Res)
