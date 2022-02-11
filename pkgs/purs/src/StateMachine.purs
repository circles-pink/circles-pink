module StateMachine where

import Prelude
import Core.State.Onboard (State(..))
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy)

type CirclesStateMachine
  = { infoGeneral ::
        { data :: Unit
        , actions ::
            { next ::
                { data :: Unit
                , toStates :: Proxy "askUserName" /\ Unit
                }
            }
        }
    , askUserName ::
        { data :: { username :: String }
        , actions ::
            { prev ::
                { data :: Unit
                , toStates :: Proxy "infoGeneral" /\ Unit
                }
            }
        }
    }
