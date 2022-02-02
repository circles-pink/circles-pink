module Core.State.Onboard where

import Prelude

data State
  = InfoGeneral
  | AskUsername
    { username :: String }
  | AskEmail
    { username :: String
    , email :: String
    , privacy :: Boolean
    , terms :: Boolean
    }
  | InfoSecurity
    { username :: String
    , email :: String
    , privacy :: Boolean
    , terms :: Boolean
    }
  | MagicWords
    { username :: String
    , email :: String
    , privacy :: Boolean
    , terms :: Boolean
    , words :: Array String
    }
  | CheckMagicWord
    { username :: String
    , email :: String
    , privacy :: Boolean
    , terms :: Boolean
    , words :: Array String
    , word :: String
    }
  | AskPhoto
    { username :: String
    , email :: String
    , privacy :: Boolean
    , terms :: Boolean
    , words :: Array String
    , url :: String
    }
  | Submit
    { username :: String
    , email :: String
    , privacy :: Boolean
    , terms :: Boolean
    , words :: Array String
    , url :: String
    }

init :: State
init = InfoGeneral

data Msg
  = SetUsername String
  | SetEmail String
  | SetPrivacy Boolean
  | SetTerms Boolean
  | SetMagicWord String
  | SetPhoto String
  | Next
  | Prev
  | Skip

type Env m
  = { apiCheckUserName :: String -> m Boolean
    , apiCheckEmail :: String -> m Boolean
    }

reducer :: forall m. Monad m => Env m -> Msg -> State -> m State
reducer env m s = case s of
  InfoGeneral -> case m of
    Next -> pure $ AskUsername { username: "" }
    _ -> pure s
  AskUsername s' -> case m of
    SetUsername n -> pure $ AskUsername { username: n }
    Next -> do
      isOk <- env.apiCheckUserName s'.username
      pure
        $ if isOk then
            AskEmail
              { email: ""
              , privacy: false
              , terms: false
              , username: s'.username
              }
          else
            s
    Prev -> pure $ InfoGeneral
    _ -> pure $ s
  AskEmail s'' -> case m of
    SetEmail e ->
      pure
        $ AskEmail
            { email: e
            , privacy: s''.privacy
            , terms: s''.terms
            , username: s''.username
            }
    SetPrivacy p ->
      pure
        $ AskEmail
            { email: s''.email
            , privacy: p
            , terms: s''.terms
            , username: s''.username
            }
    SetTerms t ->
      pure
        $ AskEmail
            { email: s''.email
            , privacy: s''.privacy
            , terms: t
            , username: s''.username
            }
    Next -> do
      isOk <- env.apiCheckEmail s''.email
      pure
        $ if isOk then
            InfoSecurity
              { email: s''.email
              , privacy: s''.privacy
              , terms: s''.terms
              , username: s''.username
              }
          else
            s
    _ -> pure $ s
  InfoSecurity _ -> case m of
    _ -> pure $ s
  MagicWords _ -> case m of
    _ -> pure $ s
  CheckMagicWord _ -> case m of
    SetMagicWord _ -> pure $ s
    _ -> pure $ s
  AskPhoto _ -> case m of
    SetPhoto _ -> pure $ s
    _ -> pure $ s
  Submit _ -> case m of
    _ -> pure $ s
