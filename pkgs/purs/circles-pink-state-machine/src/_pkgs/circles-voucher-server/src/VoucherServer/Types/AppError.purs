module VoucherServer.Types.AppError where


import VoucherServer.Prelude

import CirclesCore as CC
import Payload.Client (ClientError)
import Payload.Server.Response as Res


data AppError
  = ErrCirclesCore CCErrAll
  | ErrUnknown
  | ErrBasicAuth
  | ErrGraphQL
  | ErrGraphQLParse String
  | ErrPayloadClient ClientError
  | ErrGetVoucherAmount
  | ErrAuthChallenge

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
      + CC.ErrNotGivenOrAllowed
      + CC.ErrNullReturn
      + ()
  )

errorToFailure :: AppError -> Failure
errorToFailure = case _ of
  ErrCirclesCore _ -> internalError
  ErrUnknown -> internalError
  ErrBasicAuth -> authError
  ErrGraphQL -> internalError
  ErrGraphQLParse _ -> internalError
  ErrPayloadClient _ -> internalError
  ErrGetVoucherAmount -> internalError
  ErrAuthChallenge -> authError
  where
  authError = Error $ Res.unauthorized $
    StringBody "Authorization failed"

  internalError = Error $ Res.internalError $
    StringBody "Internal server error"

errorToLog :: AppError -> String
errorToLog = case _ of
  ErrCirclesCore e ->
    "Circles Core Error: " <> CC.printErr e
  ErrUnknown ->
    "Unknown error"
  ErrBasicAuth ->
    "Basic Authentication failed"
  ErrGraphQL ->
    "Graph QL Error"
  ErrGraphQLParse msg ->
    "Graph QL Parse Error: " <> msg
  ErrPayloadClient _ ->
    "Payload client error"
  ErrGetVoucherAmount ->
    "Failed to get Voucher Amount"
  ErrAuthChallenge ->
    "Challenge Authentication failed"
