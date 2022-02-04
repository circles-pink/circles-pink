module HTTP
  ( Req, ReqFn, Res
  ) where

import Data.Argonaut (Json)
import Data.HTTP.Method (Method)
import Effect.Aff (Aff)

type Req
  = { url :: String, method :: Method, body :: Json }

type Res
  = { status :: Int, body :: Json }

type ReqFn
  = forall a. Req -> (Res -> a) -> Aff a
