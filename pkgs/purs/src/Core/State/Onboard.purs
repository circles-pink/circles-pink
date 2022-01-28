module Core.State.Onboard where

data State
  = InfoGeneral
  | AskUsername { username :: String }
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

reducer :: Msg -> State -> State
reducer m s = case s of
  InfoGeneral -> case m of
    Next -> AskUsername { username: "" }
    _ -> s
  AskUsername s' -> case m of
    SetUsername n -> AskUsername { username: n }
    Next ->
      if true then
        AskEmail
          { email: ""
          , privacy: false
          , terms: false
          , username: s'.username
          }
      else
        s
    _ -> s
  AskEmail _ -> case m of
    SetEmail _ -> s
    SetPrivacy _ -> s
    SetTerms _ -> s
    _ -> s
  InfoSecurity _ -> case m of
    _ -> s
  MagicWords _ -> case m of
    _ -> s
  CheckMagicWord _ -> case m of
    SetMagicWord _ -> s
    _ -> s
  AskPhoto _ -> case m of
    SetPhoto _ -> s
    _ -> s
  Submit _ -> case m of
    _ -> s
