module HTTP.Milkis
  ( matchMethod
  , milkisRequest
  ) where

import Prelude
import Control.Monad.Except (ExceptT(..), except, lift)
import Data.Argonaut (stringify)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Variant (Variant)
import Effect.Aff (Aff, catchError)
import HTTP (NetworkError(..), ReqFn, _errUnknown)
import Milkis (Response, URL(..), fetch)
import Milkis as M
import Milkis.Impl (FetchImpl)
import Unsafe.Coerce (unsafeCoerce)

matchMethod :: Method -> M.Method
matchMethod = case _ of
  GET -> M.getMethod
  POST -> M.postMethod
  PUT -> M.putMethod
  DELETE -> M.deleteMethod
  _ -> M.getMethod

milkisRequest :: forall r. FetchImpl -> ReqFn r
milkisRequest fetchImpl { url, method, body } =
  fetch fetchImpl (URL url)
    { method: matchMethod method
    , body: stringify body
    }
    >>= ( \res -> do
          res' <- M.json res
          pure
            $ Right
                { body: unsafeCoerce res'
                , status: M.statusCode res
                }
      )
    # (\m -> catchError m (\_ -> pure $ Left _errUnknown))
    # ExceptT
