module VoucherServer.MonadApp.Class where

import VoucherServer.Prelude

import CirclesCore as CC
import CirclesPink.Data.Address (Address)
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Response
import VoucherServer.EnvVars (AppEnvVars)

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class
  ( MonadThrow (Response AppError) m
  , MonadAsk (AppEnv m) m
  ) <=
  MonadApp m

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

data AppError
  = ErrCirclesCore CCErrAll
  | ErrUnknown

derive instance genericVSE :: Generic AppError _
derive instance eqVSE :: Eq AppError
instance showVSE :: Show AppError where
  show = genericShow

-- Due to a compiler bug (unknown module: Partially applied type synonyms)
-- we have to redefine this type inside the current module
type CCErrAll = Variant
  ( CC.ErrParseAddress
      + CC.ErrNative
      + CC.ErrService
      + CC.ErrInvalidUrl
      + CC.ErrApi
      + ()
  )

apiErrorToFailure :: AppError -> ResponseBody
apiErrorToFailure = case _ of
  ErrCirclesCore _ ->  StringBody "INTERNAL SERVER ERROR"
  ErrUnknown -> StringBody "INTERNAL SERVER ERROR"

apiErrorToLog :: AppError -> String
apiErrorToLog = case _ of
  ErrCirclesCore _ -> "ohh!"
  ErrUnknown -> "noo!"

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

newtype AppEnv m = AppEnv
  { getTrusts :: Address -> m (Set Address)
  , envVars :: AppEnvVars
  }

