module Test.Spec.Stadium.Reflect (tests) where

import Prelude
import Data.Maybe (Maybe(..))
import Stadium.Reflect as R
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\))
import Stadium (type (||))

--------------------------------------------------------------------------------
type EmptySm
  = {}

test_empty :: TestSuite
test_empty =
  test "Empty StateMachine" do
    Assert.assert "is empty"
      $ R.reflectStateMachine (Proxy :: _ EmptySm)
      == []

--------------------------------------------------------------------------------
type NonEmptySm
  = { state1 :: { data :: Unit, actions :: {} }
    , state2 :: { data :: Unit, actions :: {} }
    }

test_nonempty :: TestSuite
test_nonempty =
  test "Nonempty StateMachine" do
    Assert.assert "is empty"
      $ R.reflectStateMachine (Proxy :: _ NonEmptySm)
      == [ "state1" /\ { data: "Unit", actions: [] }
        , "state2" /\ { data: "Unit", actions: [] }
        ]

--------------------------------------------------------------------------------
type SM
  = { state1 ::
        { data :: Int
        , actions ::
            { next ::
                { data :: String
                , to :: Proxy "state2" || "state3"
                }
            , setName ::
                { data :: String
                , to :: Proxy "state1"
                }
            }
        }
    , state2 ::
        { data :: Int
        , actions ::
            { next ::
                { data :: String
                , to :: Proxy "state2" || "state3"
                }
            , setName ::
                { data :: String
                , to :: Proxy "state1"
                }
            }
        }
    }

--------------------------------------------------------------------------------
tests :: TestSuite
tests =
  suite "Stadium.Reflect" do
    suite "reflectStateMachine" do
      test_empty
      test_nonempty
