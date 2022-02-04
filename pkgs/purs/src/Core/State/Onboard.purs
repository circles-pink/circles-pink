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

reducer :: forall m. Monad m => Env m -> (State -> m Unit) -> Msg -> State -> m Unit
reducer env setState m s = case s of
  InfoGeneral -> case m of
    Next -> setState $ AskUsername { username: "" }
    _ -> pure unit
  AskUsername s' -> case m of
    SetUsername n -> setState $ AskUsername { username: n }
    Next -> do
      isOk <- env.apiCheckUserName s'.username
      if isOk then
        setState
          $ AskEmail
              { email: ""
              , privacy: false
              , terms: false
              , username: s'.username
              }
      else
        pure unit
    Prev -> setState $ InfoGeneral
    _ -> pure unit
  AskEmail s'' -> case m of
    SetEmail e ->
      setState
        $ AskEmail
            { email: e
            , privacy: s''.privacy
            , terms: s''.terms
            , username: s''.username
            }
    SetPrivacy p ->
      setState
        $ AskEmail
            { email: s''.email
            , privacy: p
            , terms: s''.terms
            , username: s''.username
            }
    SetTerms t ->
      setState
        $ AskEmail
            { email: s''.email
            , privacy: s''.privacy
            , terms: t
            , username: s''.username
            }
    Next -> do
      isOk <- env.apiCheckEmail s''.email
      if isOk then
        setState
          $ InfoSecurity
              { email: s''.email
              , privacy: s''.privacy
              , terms: s''.terms
              , username: s''.username
              }
      else
        pure unit
    Prev -> setState $ AskUsername { username: s''.username }
    _ -> pure unit
  InfoSecurity s3' -> case m of
    Prev ->
      setState
        $ AskEmail
            { email: s3'.email
            , privacy: s3'.privacy
            , terms: s3'.terms
            , username: s3'.username
            }
    _ -> pure unit
  MagicWords _ -> case m of
    _ -> pure unit
  CheckMagicWord _ -> case m of
    SetMagicWord _ -> pure unit
    _ -> pure unit
  AskPhoto _ -> case m of
    SetPhoto _ -> pure unit
    _ -> pure unit
  Submit _ -> case m of
    _ -> pure unit
