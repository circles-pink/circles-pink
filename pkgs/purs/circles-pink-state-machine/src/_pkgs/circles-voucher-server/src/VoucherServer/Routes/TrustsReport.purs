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
import Data.Set as Set
import Payload.ResponseTypes (Response)
import Payload.Server.Response as Res
import Safe.Coerce (coerce)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.TestUtils (addrA, addrB, addrC)
import VoucherServer.MonadApp (class MonadApp, AppEnv(..), runAppTestM, testEnv)
import VoucherServer.MonadApp.Class (_getTrusts)

trustsReport
  :: forall m
   . MonadApp m
  => { guards :: { basicAuth :: Unit }
     , body :: { addresses :: Array C.Address }
     }
  -> m (Response { trusted :: Array C.Address, notTrusted :: Array C.Address })
trustsReport { body: { addresses } } = do
  AppEnv { getTrusts, envVars } <- ask
  xbgeTrusts <- getTrusts $ coerce envVars.xbgeSafeAddress

  let { yes, no } = A.partition (_ `Set.member` xbgeTrusts) addresses

  pure $ Res.ok { trusted: yes, notTrusted: no }

spec :: Spec Unit
spec = do
  describe "Route trustsReport" do
    let
      env = testEnv
        # set _getTrusts (\_ -> pure $ Set.fromFoldable [ addrA, addrB ])

    it "returns the trusted and untrusted addresses" do
      trustsReport { 
        guards: {basicAuth : unit},
        body: { addresses: [ addrA, addrB, addrC ] } }
        # runAppTestM env
        # shouldEqual
        $ Right
        $ Res.ok { trusted: [ addrA, addrB ], notTrusted: [ addrC ] }