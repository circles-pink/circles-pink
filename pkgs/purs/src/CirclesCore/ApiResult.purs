module CirclesCore.ApiResult
  ( ApiError
  , ApiResult
  , apiResultToEither
  ) where

import Prelude
import Data.Either (Either(..))
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Object (Object)
import Foreign.Object.Unsafe (unsafeIndex)

type ApiError
  = { message :: String, code :: Int }

newtype ApiResult :: forall k. k -> Type
newtype ApiResult a
  = ApiResult (Object Foreign)

apiResultToEither :: forall a. ApiResult a -> Either ApiError a
apiResultToEither (ApiResult fo) =
  let
    status = unsafeIndex fo "status" # unsafeFromForeign
  in
    if status == "ok" then
      let
        data_ = unsafeIndex fo "data" # unsafeFromForeign
      in
        Right data_
    else
      let
        code = unsafeIndex fo "code" # unsafeFromForeign

        message = unsafeIndex fo "message" # unsafeFromForeign
      in
        Left { code, message }
