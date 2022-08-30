module VoucherServer.Main
  ( main
  ) where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Timer (setInterval)
import Node.Process (getEnv)
import Payload.ResponseTypes (Failure)
import Payload.Server (defaultOpts)
import Payload.Server as Payload
import VoucherServer.EnvVars (AppEnvVars(..), parseEnvVars)
import VoucherServer.Guards.Auth (basicAuthGuard)
import VoucherServer.Monad.AppM (AppM, runAppM)
import VoucherServer.Monad.AppM.AppEnv as Prod
import VoucherServer.Monad.Init (InitM, runInitM)
import VoucherServer.Monad.MkAppM (runMkAppM)
import VoucherServer.Routes.GetVoucherProviders (routeGetVoucherProviders) as Routes
import VoucherServer.Routes.GetVouchers (routeGetVouchers) as Routes
import VoucherServer.Routes.TrustCount (trustCount) as Routes
import VoucherServer.Routes.TrustUsers (trustUsers) as Routes
import VoucherServer.Routes.TrustsReport (trustsReport) as Routes
import VoucherServer.Spec (spec)
import VoucherServer.Sync as Sync
import VoucherServer.Types.AppConstants (AppConstants(..))
import VoucherServer.Types.AppError (AppError(..), errorToFailure, errorToLog)
import VoucherServer.Types.Envs (AppEnv)

--------------------------------------------------------------------------------

type M a = InitM a

constants :: AppConstants
constants = AppConstants
  { trustLimitPercentage: 100.0
  , authChallengeDuration: Seconds 60.0
  }

getEnvVars :: M AppEnvVars
getEnvVars = do
  obj <- getEnv # liftEffect

  parseEnvVars obj
    # lmap ErrParseEnv
    # liftEither

app :: M Unit
app = do
  envVars@(AppEnvVars env) <- getEnvVars

  prodEnv <- Prod.mkAppEnv
    # runMkAppM
        { envVars
        , constants
        }
    # liftAff
    >>= liftEither

  let
    handlers =
      { getVouchers:
          Routes.routeGetVouchers
            >>> runRoute prodEnv
      , getVoucherProviders:
          Routes.routeGetVoucherProviders
            >>> runRoute prodEnv
      , trustUsers:
          Routes.trustUsers
            >>> runRoute prodEnv
      , trustsReport:
          Routes.trustsReport
            >>> runRoute prodEnv
      , trustCount:
          Routes.trustCount
            >>> runRoute prodEnv
      }

  let
    guards =
      { basicAuth:
          basicAuthGuard
            >>> runRoute prodEnv
      }

  _ <- liftEffect $ setInterval 5000 (launchAff_ $ runSync prodEnv Sync.syncVouchers)

  _ <-
    Payload.startGuarded (defaultOpts { port = env.port })
      spec
      { guards, handlers }
      # liftAff
      <#> lmap ErrServer
      >>= liftEither

  pure unit

runRoute :: forall a. AppEnv AppM -> AppM a -> Aff (Either Failure a)
runRoute env x = do
  result <- runAppM env x
  case result of
    Left appError -> do
      log ("Route Error: " <> errorToLog appError)
      pure $ Left $ errorToFailure appError
    Right ok -> pure $ Right ok

runSync :: forall a. AppEnv AppM -> AppM a -> Aff Unit
runSync env x = do
  result <- runAppM env x
  case result of
    Left appError -> do
      log ("Syncing Error: " <> errorToLog appError)
    Right _ -> pure unit

runM :: forall a. M a -> Aff Unit
runM x = do
  result <- runInitM constants x
  case result of
    Left err ->
      log ("Server Inititialization error: " <> errorToLog err)
    Right _ -> pure unit

main :: Effect Unit
main = launchAff_ $ runM app
