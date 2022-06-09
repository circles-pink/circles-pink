module CirclesPink.Garden.StateMachine.ProtocolDef.Common where

import CirclesPink.Garden.StateMachine.Control.Env as Env
import Type.Row (type (+))

type ErrLoginTask r = Env.ErrUserResolve
  + Env.ErrGetSafeStatus
  + Env.ErrTrustGetNetwork
  + Env.ErrIsTrusted
  + Env.ErrIsFunded
  + Env.ErrInvalidMnemonic
  + r
