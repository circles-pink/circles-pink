module VoucherServer.MonadVoucherServer where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Payload.ResponseTypes (Failure(..), ResponseBody(..))
import Payload.Server.Response as Response

data VoucherServerError = Foo

apiErrorToFailure :: VoucherServerError -> Failure
apiErrorToFailure = case _ of
  Foo -> Error $ Response.internalError (StringBody "INTERNAL SERVER ERROR")

apiErrorToLog :: VoucherServerError -> String
apiErrorToLog = case _ of
  Foo -> "ohh!"

class (MonadThrow VoucherServerError m) <= MonadVoucherServer m

