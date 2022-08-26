module VoucherServer.Main
  ( main
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import Effect.Timer (setInterval)
import Node.Process (exit, getEnv)
import Payload.ResponseTypes (Failure)
import Payload.Server (defaultOpts)
import Payload.Server as Payload
import Type.Proxy (Proxy(..))
import TypedEnv (envErrorMessage, fromEnv)
import VoucherServer.EnvVars (AppEnvVarsSpec)
import VoucherServer.Guards.Auth (basicAuthGuard)
import VoucherServer.MonadApp (AppEnv, AppProdM, errorToLog, runAppProdM)
import VoucherServer.MonadApp.Class (AppConstants, errorToFailure)
import VoucherServer.MonadApp.Impl.Prod.AppEnv as Prod
import VoucherServer.MonadApp.Impl.Prod.MkAppProdM (runMkAppProdM)
import VoucherServer.Routes.GetVouchers (routeGetVouchers) as Routes
import VoucherServer.Routes.TrustCount (trustCount) as Routes
import VoucherServer.Routes.TrustUsers (trustUsers) as Routes
import VoucherServer.Routes.TrustsReport (trustsReport) as Routes
import VoucherServer.Routes.GetVoucherProviders (routeGetVoucherProviders) as Routes
import VoucherServer.Spec (spec)
import VoucherServer.Sync as Sync

--------------------------------------------------------------------------------

app :: Aff (Either String Unit)
app = do
  env <- liftEffect $ getEnv
  let
    config = fromEnv (Proxy :: _ AppEnvVarsSpec) env
      # lmap envErrorMessage
  case config of
    Left e -> do
      error e
      liftEffect $ exit 1
    Right parsedEnv -> do
      prodEnv_ <- Prod.mkAppEnv # runMkAppProdM
        { envVars: parsedEnv
        , constants: appConstants
        }
      case prodEnv_ of
        Left e -> do
          error $ errorToLog e
          liftEffect $ exit 1
        Right prodEnv -> do
          let
            handlers =
              { getVouchers: Routes.routeGetVouchers
                  >>> runRoute prodEnv
              , getVoucherProviders: Routes.routeGetVoucherProviders
                  >>> runRoute prodEnv
              , trustUsers: Routes.trustUsers
                  >>> runRoute prodEnv
              , trustsReport: Routes.trustsReport
                  >>> runRoute prodEnv
              , trustCount: Routes.trustCount
                  >>> runRoute prodEnv
              }
            guards =
              { basicAuth: basicAuthGuard >>> runRoute prodEnv
              }
          _ <- liftEffect $ setInterval 5000 (launchAff_ $ runSync prodEnv Sync.syncVouchers)
          _ <- Payload.startGuarded (defaultOpts { port = fromMaybe 4000 parsedEnv.port })
            spec
            { guards, handlers }
          pure $ Right unit

runRoute :: forall a. AppEnv AppProdM -> AppProdM a -> Aff (Either Failure a)
runRoute env x = do
  result <- runAppProdM env x
  case result of
    Left appError -> do
      log ("Route Error: " <> errorToLog appError)
      pure $ Left $ errorToFailure appError
    Right ok -> pure $ Right ok

runSync :: forall a. AppEnv AppProdM -> AppProdM a -> Aff Unit
runSync env x = do
  result <- runAppProdM env x
  case result of
    Left appError -> do
      log ("Syncing Error: " <> errorToLog appError)
    Right _ -> pure unit

appConstants :: AppConstants
appConstants =
  { trustLimitPercentage: 100.0
  , authChallengeDuration: Seconds 60.0
  }

main :: Effect Unit
main = launchAff_ app
