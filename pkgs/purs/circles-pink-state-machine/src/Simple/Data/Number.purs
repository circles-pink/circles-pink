module Simple.Data.Number where

import Prelude as P

import Simple.Data.Eq (Eq)

eq :: Eq Number
eq = { eq: P.eq }