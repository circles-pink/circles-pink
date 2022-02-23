module CirclesPink.StateMachine.Control
  ( Env
  , circlesControl
  ) where

import Prelude
import CirclesPink.StateMachine (_circlesStateMachine)
import CirclesPink.StateMachine.Action (CirclesAction)
import CirclesPink.StateMachine.State (CirclesState)
import CirclesPink.StateMachine.State as S
import CirlesPink.StateMachine.Error (CirclesError)
import Data.Either (Either)
import Network.RemoteData (RemoteData(..))
import Stadium.Control as C
import Undefined (undefined)

type Env m
  = { apiCheckUserName ::
        String ->
        m
          ( Either
              CirclesError
              { username :: String, isValid :: Boolean }
          )
    , apiCheckEmail :: String -> m Boolean
    }

circlesControl :: forall m. Monad m => Env m -> ((CirclesState -> CirclesState) -> m Unit) -> CirclesState -> CirclesAction -> m Unit
circlesControl env =
  C.mkControl
    _circlesStateMachine
    { infoGeneral:
        { next: \set _ _ -> set $ \st -> S._askUsername st }
    , askUsername:
        { prev: \set _ _ -> set $ \st -> S._infoGeneral st
        , setUsername:
            \set _ x -> do
              set $ \st -> S._askUsername st { username = x }
        -- set $ \st -> S._askUsername st { usernameApiResult = Loading }
        -- result <- env.apiCheckUserName x
        -- set
        --   $ \st -> undefined
        -- if result.username == st.username then
        --   S._askUsername
        --     st
        --       { usernameApiResult =
        --         if result.isValid then
        --           Success unit
        --         else
        --           Failure unit
        --       }
        -- else
        --   S._askUsername st
        }
    }
