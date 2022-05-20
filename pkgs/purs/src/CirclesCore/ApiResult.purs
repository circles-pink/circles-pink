module CirclesCore.ApiResult
  ( ApiError
  , ApiResult(..)
  , apiResultToEither
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Foreign (Foreign, unsafeFromForeign)
import Foreign.Object (Object)
import Foreign.Object.Unsafe (unsafeIndex)
import Structural (class Structural)

type ApiError = { message :: String, code :: Int }

--------------------------------------------------------------------------------
derive instance newtypeApiResult :: Newtype (ApiResult a) _

derive newtype instance structuralApiResult :: Structural a => Structural (ApiResult a)

newtype ApiResult :: forall k. k -> Type
newtype ApiResult a = ApiResult (Object Foreign)

--------------------------------------------------------------------------------
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
