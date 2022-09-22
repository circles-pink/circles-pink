module Simple.Data.Duration where

import Data.Time.Duration (Milliseconds(..))

unMilliseconds :: Milliseconds -> Number
unMilliseconds (Milliseconds x) = x