module CirclesPink.Garden.CirclesCore
  ( Err
  , ErrNative
  , ErrService
  , UserOptions
  , module Exp
  , newCirclesCore
  , newWeb3
  , newWebSocketProvider
  , printErr
  , privKeyToAccount
  , safePredictAddress
  , safePrepareDeploy
  , userRegister
  ) where

import Prelude
import CirclesPink.Garden.CirclesCore.Bindings (Options, Provider, Web3, CirclesCore, Account) as Exp
import CirclesPink.Garden.CirclesCore.Bindings as B
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Variant (Variant, case_, inj, on)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error, message, try)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Wallet.PrivateKey (Address, Nonce, PrivateKey, addrToString, nonceToBigInt)
import Wallet.PrivateKey as P

newWebSocketProvider :: forall r. String -> ExceptV (ErrNative + r) Effect B.Provider
newWebSocketProvider x1 =
  B.newWebSocketProvider x1
    # try
    <#> lmap (inj (Proxy :: _ "errNative"))
    # ExceptT

newWeb3 :: B.Provider -> Effect B.Web3
newWeb3 = B.newWeb3

newCirclesCore :: forall r. B.Web3 -> B.Options -> ExceptV (ErrNative + r) Effect B.CirclesCore
newCirclesCore x1 x2 =
  B.newCirclesCore x1 x2
    # try
    <#> lmap (inj (Proxy :: _ "errNative"))
    # ExceptT

privKeyToAccount :: forall r. B.Web3 -> PrivateKey -> ExceptV (ErrNative + r) Effect B.Account
privKeyToAccount w3 pk =
  B.privKeyToAccount w3 (P.toString pk)
    # try
    <#> lmap (inj (Proxy :: _ "errNative"))
    # ExceptT

safePredictAddress :: forall r. B.CirclesCore -> B.Account -> { nonce :: P.Nonce } -> ExceptV (ErrNative + r) Aff P.Address
safePredictAddress cc ac opts =
  B.safePredictAddress cc ac { nonce: P.nonceToBigInt opts.nonce }
    <#> P.unsafeAddrFromString
    # attempt
    <#> lmap (inj (Proxy :: _ "errNative"))
    # ExceptT

safePrepareDeploy :: forall r. B.CirclesCore -> B.Account -> { nonce :: P.Nonce } -> ExceptV (ErrNative + r) Aff P.Address
safePrepareDeploy cc ac opts =
  B.safePrepareDeploy cc ac { nonce: P.nonceToBigInt opts.nonce }
    <#> P.unsafeAddrFromString
    # attempt
    <#> lmap (inj (Proxy :: _ "errNative"))
    # ExceptT

type ErrNative r
  = ( errNative :: Error | r )

type ErrService r
  = ( errService :: Unit | r )

type Err r
  = ErrNative + ErrService + r

printErr :: Variant (Err ()) -> String
printErr =
  case_
    # on (Proxy :: _ "errNative") (\e -> "Native Error: " <> message e)
    # on (Proxy :: _ "errService") (\_ -> "service error")

type UserOptions
  = { nonce :: Nonce
    , safeAddress :: Address
    , username :: String
    , email :: String
    }

userRegister :: forall r. B.CirclesCore -> B.Account -> UserOptions -> ExceptV (ErrService + ErrNative + r) Aff Unit
userRegister cc ac opts =
  B.userRegister cc ac
    { nonce: nonceToBigInt opts.nonce
    , safeAddress: addrToString opts.safeAddress
    , username: opts.username
    , email: opts.email
    }
    # attempt
    <#> lmap (inj (Proxy :: _ "errNative"))
    <#> (\e -> e >>= \b -> if b then Right unit else Left $ inj (Proxy :: _ "errService") unit)
    # ExceptT
