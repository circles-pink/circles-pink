module VoucherServer.Types.AppConstants where

import VoucherServer.Prelude

import Data.Time.Duration (Seconds(..))
import Safe.Coerce (coerce)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)


newtype AppConstants = AppConstants
  { trustLimitPercentage :: Number
  , authChallengeDuration :: Seconds
  }

instance Arbitrary AppConstants where
  arbitrary = ado
    trustLimitPercentage <- arbitrary
    authChallengeDuration <- coerce <$> (arbitrary :: Gen Number)
    in
      AppConstants
        { trustLimitPercentage
        , authChallengeDuration
        }

