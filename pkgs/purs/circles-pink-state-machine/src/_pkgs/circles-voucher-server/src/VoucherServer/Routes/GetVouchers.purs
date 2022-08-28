module VoucherServer.Routes.GetVouchers where

import Prelude

import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Reader (ask)
import Data.DateTime (DateTime, diff)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Number as Num
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Safe.Coerce (coerce)
import VoucherServer.Crypto (decrypt)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.MonadApp.Class (class MonadApp, getResponseData)
import VoucherServer.Spec.Types (Voucher(..), VoucherCode(..), VoucherCodeEncrypted(..), VoucherEncrypted(..))
import VoucherServer.Specs.Xbge (Address(..))
import VoucherServer.Types.AppConstants (AppConstants(..))
import VoucherServer.Types.AppError (AppError(..))
import VoucherServer.Types.Envs (AppEnv(..), CirclesCoreEnv(..), XbgeClientEnv(..))
import Web3 (Message(..), SignatureObj(..))
import Web3.Pure as W3

routeGetVouchers :: forall m. MonadApp m => { body :: { signatureObj :: SignatureObj } } -> m (Array Voucher)
routeGetVouchers { body: { signatureObj } } = do
  AppEnv
    { xbgeClient: XbgeClientEnv { getVouchers }
    , envVars: AppEnvVars { voucherCodeSecret }
    } <- ask

  safeAddress <- authChallenge signatureObj

  vouchers <-
    getVouchers
      { query: { safeAddress: Just safeAddress }
      }
      <#> getResponseData

  for vouchers $
    decryptVoucher voucherCodeSecret >>> liftMaybe ErrUnknown

authChallenge :: forall m. MonadApp m => SignatureObj -> m Address
authChallenge sigObj@(SignatureObj { message, messageHash }) = do
  AppEnv
    { circlesCore: CirclesCoreEnv { getSafeAddress }
    , constants: AppConstants { authChallengeDuration }
    , now
    } <- ask

  timestamp <- toDateTime <$> now

  let
    isMessageValid = messageHash == W3.accountsHashMessage message

  unless isMessageValid $ throwError ErrAuthChallenge

  msgTime <- messageToDateTime message # liftMaybe ErrAuthChallenge

  let
    isInTimeFrame = diff msgTime timestamp <= authChallengeDuration

  unless isInTimeFrame $ throwError ErrAuthChallenge

  address <- W3.accountsRecover sigObj # liftMaybe ErrAuthChallenge

  getSafeAddress $ coerce address

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

messageToDateTime :: Message -> Maybe DateTime
messageToDateTime msg = msg
  # un Message
  # Num.fromString
  <#> Milliseconds
  >>= instant
  <#> toDateTime

decryptVoucher :: String -> VoucherEncrypted -> Maybe Voucher
decryptVoucher key (VoucherEncrypted x) = ado
  code <- decryptVoucherCode key x.code
  in Voucher x { code = code }

decryptVoucherCode :: String -> VoucherCodeEncrypted -> Maybe VoucherCode
decryptVoucherCode key (VoucherCodeEncrypted s) = decrypt key s <#> VoucherCode
