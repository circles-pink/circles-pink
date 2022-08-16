module VoucherServer.Routes.TrustsReport
  ( trustsReport
  ) where

import Prelude

import CirclesPink.Data.Address as C
import Control.Monad.Reader (ask)
import Data.Array as A
import Data.Either (Either(..))
import Data.Set as Set
import Debug.Extra (todo)
import Debug.Extra (todo)
import Payload.ResponseTypes (Response(..))
import Payload.Server.Response as Res
import Safe.Coerce (coerce)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import VoucherServer.MonadApp (class MonadApp, AppEnv(..), runAppTestM, testEnv)

trustsReport
  :: forall m
   . MonadApp m
  => { body :: { addresses :: Array C.Address } }
  -> m (Response { trusted :: Array C.Address, notTrusted :: Array C.Address })
trustsReport { body: { addresses } } = do
  AppEnv { getTrusts, envVars } <- ask
  xbgeTrusts <- getTrusts $ coerce envVars.xbgeSafeAddress

  let { yes, no } = A.partition (_ `Set.member` xbgeTrusts) addresses

  pure $ Res.ok { trusted: yes, notTrusted: no }

spec :: Spec Unit
spec =
  describe "Route trustsReport" do
    it "foo" do
      trustsReport { body: { addresses: [] } }
        # runAppTestM testEnv
        # shouldEqual
        $ Right
        $ Res.ok { trusted: [], notTrusted: [] }