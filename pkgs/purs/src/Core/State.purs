module Core.State where

import Core.State.Onboard as O

data State
  = Init
  | Login
  | Onboard O.State
