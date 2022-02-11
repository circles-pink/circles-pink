module StateMachine.Types
  ( Action
  , AskEmail
  , AskUserName
  , InfoGeneral
  , InfoSecurity
  , Protocol
  , State
  , init
  ) where

import Prelude
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant, inj, match)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------
init :: State
init = inj (Proxy :: _ "infoGeneral") unit

--------------------------------------------------------------------------------
-- Destructors
--------------------------------------------------------------------------------
matchState ::
  forall z.
  { infoGeneral :: InfoGeneral -> z
  , askUserName :: AskUserName -> z
  , askEmail :: AskEmail -> z
  , infoSecurity :: InfoSecurity -> z
  } ->
  State -> z
matchState = match

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type InfoGeneral
  = Unit

type AskUserName
  = { username :: String }

type AskEmail
  = { username :: String
    , email :: String
    , privacy :: Boolean
    , terms :: Boolean
    }

type InfoSecurity
  = { username :: String
    , email :: String
    , privacy :: Boolean
    , terms :: Boolean
    }

type State
  = Variant
      ( infoGeneral :: InfoGeneral
      , askUserName :: AskUserName
      , askEmail :: AskEmail
      , infoSecurity :: InfoSecurity
      -- , magicWords ::
      --     { username :: String
      --     , email :: String
      --     , privacy :: Boolean
      --     , terms :: Boolean
      --     , words :: Array String
      --     }
      -- , checkMagicWord ::
      --     { username :: String
      --     , email :: String
      --     , privacy :: Boolean
      --     , terms :: Boolean
      --     , words :: Array String
      --     , word :: String
      --     }
      -- , askPhoto ::
      --     { username :: String
      --     , email :: String
      --     , privacy :: Boolean
      --     , terms :: Boolean
      --     , words :: Array String
      --     , url :: String
      --     }
      -- , submit ::
      --     { username :: String
      --     , email :: String
      --     , privacy :: Boolean
      --     , terms :: Boolean
      --     , words :: Array String
      --     , url :: String
      --     }
      )

type Action
  = Variant
      ( infoGeneral ::
          Variant
            ( next :: Unit )
      , askUserName ::
          Variant
            ( prev :: Unit
            , next :: Unit
            , setUserName :: String
            )
      , askEmail ::
          Variant
            ( prev :: Unit
            , next :: Unit
            , setEmail :: String
            , setTerms :: Boolean
            , setPrivacy :: Boolean
            )
      , infoSecurity ::
          Variant
            ( prev :: Unit )
      -- , magicWords :: Variant ()
      -- , checkMagicWord ::
      --     Variant
      --       ( setMagicWord :: String )
      -- , askPhoto ::
      --     Variant
      --       ( setPhoto :: String )
      -- , submit :: Variant ()
      )

type Protocol
  = { infoGeneral ::
        { actions ::
            { next :: { toStates :: Proxy "askUserName" /\ Unit }
            }
        }
    , askUserName ::
        { actions ::
            { prev :: { toStates :: Proxy "infoGeneral" /\ Unit }
            , next :: { toStates :: Proxy "askEmail" /\ Unit }
            , setUserName :: { toStates :: Proxy "askUserName" /\ Unit }
            }
        }
    , askEmail ::
        { actions ::
            { prev :: { toStates :: Proxy "askUserName" /\ Unit }
            , next :: { toStates :: Proxy "infoSecurity" /\ Unit }
            , setEmail :: { toStates :: Proxy "askEmail" /\ Unit }
            , setTerms :: { toStates :: Proxy "askEmail" /\ Unit }
            , setPrivacy :: { toStates :: Proxy "askEmail" /\ Unit }
            }
        }
    , infoSecurity ::
        { actions ::
            { prev :: { toStates :: Proxy "askEmail" /\ Unit }
            }
        }
    -- , magicWords :: { actions :: {} }
    -- , checkMagicWord ::
    --     { actions ::
    --         { setMagicWord :: { toStates :: Proxy "checkMagicWord" /\ Unit }
    --         }
    --     }
    -- , askPhoto ::
    --     { actions ::
    --         { setPhoto :: { toStates :: Proxy "askPhoto" /\ Unit }
    --         }
    --     }
    -- , submit :: { actions :: {} }
    }
