module VoucherServer.Routes.TrustUser where

import Prelude

import CirclesPink.Data.Address (Address)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Payload.ResponseTypes (Failure)


trustUsers :: {-ServerEnv ->-} { body :: { safeAddresses :: Array Address } } -> Aff (Either Failure {})
trustUsers {} = pure $ Right {}