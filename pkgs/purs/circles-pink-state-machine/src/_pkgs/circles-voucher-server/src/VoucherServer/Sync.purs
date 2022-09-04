module VoucherServer.Sync where

import Prelude

import Control.Monad.Error.Class (catchError, liftMaybe, throwError)
import Control.Monad.Reader (ask)
import Data.Array as A
import Data.BN (BN)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.MonadApp.Class (class MonadApp, getResponseData, log, scope)
import VoucherServer.Spec.Types (EurCent(..), Freckles(..), VoucherAmount(..), VoucherEncrypted(..), VoucherOffer(..), VoucherProvider(..), VoucherProviderId(..))
import VoucherServer.Specs.Xbge (Address)
import VoucherServer.Types (Transfer(..), TransferMeta(..))
import VoucherServer.Types.AppError (AppError(..))
import VoucherServer.Types.AppLog (AppLog(..))
import VoucherServer.Types.AppScope (AppScope(..))
import VoucherServer.Types.Envs (AppEnv(..), CirclesCoreEnv(..), GraphNodeEnv(..), XbgeClientEnv(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Threshold a = Threshold { above :: a, below :: a }

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

threshold :: Threshold EurCent
threshold = Threshold { above: EurCent 5, below: EurCent 5 }

--------------------------------------------------------------------------------
-- Sync
--------------------------------------------------------------------------------

syncVouchers :: forall m. MonadApp m => m Unit
syncVouchers =
  scope AtSync do

    AppEnv
      { graphNode: GraphNodeEnv { getTransactions }
      , xbgeClient: XbgeClientEnv { getVouchers }
      , envVars: AppEnvVars { xbgeSafeAddress }
      } <- ask

    log LogSyncStart

    txs <- getTransactions { toAddress: xbgeSafeAddress }

    log $ LogSyncFetchedTxs $ A.length txs

    vouchers <- getVouchers { query: { safeAddress: Nothing } }
      <#> getResponseData

    let
      vouchersLookup = vouchers
        <#> (\all@(VoucherEncrypted { sold: { transactionId } }) -> transactionId /\ all)
        # M.fromFoldable

    let
      unfinalizedTxs = txs # A.filter
        \(Transfer { id }) -> not $ M.member id vouchersLookup

    for_ unfinalizedTxs
      (\tx -> (void $ finalizeTx tx) `catchError` (log <<< LogCatchedFinalizeTxError))

    log LogSyncEnd

finalizeTx :: forall m. MonadApp m => Transfer -> m VoucherEncrypted
finalizeTx transfer@(Transfer { from, amount, id }) =
  scope (AtFinalizeTx transfer) do

    AppEnv
      { graphNode: GraphNodeEnv { getTransferMeta }
      , circlesCore: CirclesCoreEnv { getPaymentNote }
      , xbgeClient: XbgeClientEnv
          { getVoucherProviders
          , finalizeVoucherPurchase
          }
      } <- ask

    log $ LogStartFinalizeTx transfer

    TransferMeta { time, transactionHash } <- getTransferMeta id

    providers <- getVoucherProviders {}
      <#> getResponseData

    let eur = frecklesToEurCent time amount

    providerId <- getPaymentNote transactionHash <#> VoucherProviderId

    voucherAmount <-
      ( getVoucherAmount providers providerId eur
          # liftMaybe ErrGetVoucherAmount
      )
        `catchError` \err -> do
          redeemAmount from eur
          throwError err

    voucher <-
      finalizeVoucherPurchase
        { body:
            { safeAddress: from
            , providerId
            , amount: voucherAmount
            , transactionId: id
            }
        }
        <#> getResponseData

    log $ LogFinishFinalizeTx voucher

    pure voucher

redeemAmount :: forall m. MonadApp m => Address -> EurCent -> m Unit
redeemAmount _ _ = do
  log LogRedeem
  pure unit

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

getVoucherAmount :: Array VoucherProvider -> VoucherProviderId -> EurCent -> Maybe VoucherAmount
getVoucherAmount providers providerId payedAmount = do
  (VoucherProvider provider) <- A.find (\(VoucherProvider p) -> p.id == providerId) providers
  (VoucherOffer { amount }) <- A.find (\(VoucherOffer { amount: (VoucherAmount amount) }) -> almostEquals threshold payedAmount amount) provider.availableOffers
  pure amount

almostEquals :: Threshold EurCent -> EurCent -> EurCent -> Boolean
almostEquals
  (Threshold { above: (EurCent above), below: (EurCent below) })
  (EurCent amount)
  (EurCent price) =
  let
    isInLowerRange = amount >= (price * 100 - below)
    isInUpperRange = amount <= (price * 100 + above)
  in
    isInLowerRange && isInUpperRange

frecklesToEurCent :: Instant -> Freckles -> EurCent
frecklesToEurCent timestamp (Freckles freckles) =
  let
    ms = unInstant timestamp
  in
    frecklesToEuroCentImpl (unwrap ms) freckles # EurCent

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

foreign import frecklesToEuroCentImpl :: Number -> BN -> Int