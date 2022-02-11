module StateMachine.Protocol
  ( AskEmail
  , AskUserName
  , CirclesStateMachine
  , InfoGeneral
  , State
  ) where

import Prelude
import Core.State.Onboard (State(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Variant (Variant)
import Type.Proxy (Proxy)

type InfoGeneral
  = Unit

type AskUserName
  = { username :: String }

type AskEmail
  = { email :: String }

type State
  = Variant
      ( infoGeneral :: InfoGeneral
      , askUserName :: AskUserName
      , askEmail :: AskEmail
      )

-- type Action
--   = Variant
--       ( infoGeneral :: InfoGeneral
--       , askUserName :: AskUserName
--       , askEmail :: AskEmail
--       )
type CirclesStateMachine
  = { infoGeneral ::
        { data :: InfoGeneral
        , actions ::
            { next :: { data :: Unit, toStates :: Proxy "askUserName" /\ Unit }
            }
        }
    , askUserName ::
        { data :: AskUserName
        , actions ::
            { prev :: { data :: Unit, toStates :: Proxy "infoGeneral" /\ Unit }
            , next :: { data :: Unit, toStates :: Proxy "askEmail" /\ Unit }
            , setUserName :: { data :: String, toStates :: Proxy "askUserName" /\ Unit }
            }
        }
    , askEmail ::
        { data :: AskEmail
        , actions ::
            { prev :: { data :: Unit, toStates :: Proxy "askUserName" /\ Unit }
            , setEmail :: { data :: String, toStates :: Proxy "askEmail" /\ Unit }
            , setTerms :: { data :: String, toStates :: Proxy "askEmail" /\ Unit }
            , setPrivacy :: { data :: String, toStates :: Proxy "askEmail" /\ Unit }
            }
        }
    }
