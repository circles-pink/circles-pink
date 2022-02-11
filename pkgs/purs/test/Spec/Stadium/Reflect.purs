module Test.Spec.Stadium.Reflect (tests) where

import Prelude
import Data.Maybe (Maybe(..))
import Stadium.Reflect as R
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested ((/\), type (/\))
import Stadium (type (||))

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
      test "Empty StateMachine" do
        let
          smProxy = Proxy :: _ {}
        Assert.assert "is empty"
          $ R.reflectStateMachine smProxy
          == []
      test "Nonempty StateMachine" do
        let
          smProxy =
            Proxy ::
              _
                { state1 :: { data :: Unit, actions :: {} }
                , state2 :: { data :: Unit, actions :: {} }
                }
        Assert.assert "has states"
          $ R.reflectStateMachine smProxy
          == [ "state1" /\ { data: "Unit", actions: [] }
            , "state2" /\ { data: "Unit", actions: [] }
            ]
      test "With action" do
        let
          smProxy =
            Proxy ::
              _
                { state1 ::
                    { data :: Unit
                    , actions ::
                        { action1 :: { data :: Unit, toStates :: Unit }
                        }
                    }
                }
        Assert.assert "has action"
          $ R.reflectStateMachine smProxy
          == [ "state1"
                /\ { data: "Unit"
                  , actions:
                      [ "action1" /\ { data: "Unit", toStates: [] }
                      ]
                  }
            ]
      test "With action and toStates" do
        let
          smProxy =
            Proxy ::
              _
                { state1 ::
                    { data :: Unit
                    , actions ::
                        { action1 ::
                            { data :: Unit
                            , toStates :: Proxy "state1" /\ Proxy "state2" /\ Unit
                            }
                        , action2 ::
                            { data :: Unit
                            , toStates :: Unit
                            }
                        }
                    }
                , state2 ::
                    { data :: Unit
                    , actions :: {}
                    }
                }
        Assert.assert "has action with states"
          $ R.reflectStateMachine smProxy
          == [ "state1"
                /\ { data: "Unit"
                  , actions:
                      [ "action1" /\ { data: "Unit", toStates: [ "state1", "state2" ] }
                      , "action2" /\ { data: "Unit", toStates: [] }
                      ]
                  }
            , "state2"
                /\ { data: "Unit"
                  , actions: []
                  }
            ]
