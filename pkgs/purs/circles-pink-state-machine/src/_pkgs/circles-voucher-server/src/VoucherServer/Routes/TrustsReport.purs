module VoucherServer.Routes.TrustsReport
  ( trustsReport
  , spec
  ) where

import Prelude

import CirclesPink.Data.Address as C
import Control.Monad.Reader (ask)
import Data.Array as A
import Data.Either (Either(..))
import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.Set as Set
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Res
import Safe.Coerce (coerce)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.TestUtils (addrA, addrB, addrC)
import VoucherServer.EnvVars (AppEnvVars(..))
import VoucherServer.MonadApp (class MonadApp, AppEnv(..), runAppTestM, testEnv)
import VoucherServer.MonadApp.Class (CirclesCoreEnv(..), _AppEnv, _CirclesCoreEnv, _circlesCore, _getTrusts)

--------------------------------------------------------------------------------
-- Route
--------------------------------------------------------------------------------

trustsReport
  :: forall m
   . MonadApp m
  => { guards :: { basicAuth :: Unit }
     , body :: { addresses :: Array C.Address }
     }
  -> m (Response { trusted :: Array C.Address, notTrusted :: Array C.Address })
trustsReport { body: { addresses } } = do
  AppEnv
    { circlesCore: CirclesCoreEnv { getTrusts }
    , envVars: AppEnvVars envVars
    } <- ask
  xbgeTrusts <- getTrusts $ coerce envVars.xbgeSafeAddress

  let { yes, no } = A.partition (_ `Set.member` xbgeTrusts) addresses

  pure $ Res.ok { trusted: yes, notTrusted: no }

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "Route trustsReport" do
    let
      env = testEnv
        # set (_AppEnv <<< prop _circlesCore <<< _CirclesCoreEnv <<< prop _getTrusts) (\_ -> pure $ Set.fromFoldable [ addrA, addrB ])

    it "returns the trusted and untrusted addresses" do
      trustsReport
        { guards: { basicAuth: unit }
        , body: { addresses: [ addrA, addrB, addrC ] }
        }
        # runAppTestM env
        # shouldEqual
        $ Right
        $ Res.ok { trusted: [ addrA, addrB ], notTrusted: [ addrC ] }