module Web3
  ( newWeb3
  , newWebSocketProvider
  , sendTransaction
  ) where

import Prelude
import CirclesPink.URI (URI)
import CirclesPink.URI as U
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Bifunctor (lmap)
import Data.Maybe (fromJust)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, message, try)
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Exception (name)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Web3.Bindings as B

--------------------------------------------------------------------------------
-- Error types
--------------------------------------------------------------------------------
type NativeError
  = { message :: String
    , name :: String
    }

type ErrNative r
  = ( errNative :: NativeError | r )

--------------------------------------------------------------------------------
-- Err constructors
--------------------------------------------------------------------------------
_errNative :: forall r. NativeError -> Variant (ErrNative r)
_errNative = inj (Proxy :: _ "errNative")

--------------------------------------------------------------------------------
-- Send Transaction
--------------------------------------------------------------------------------
type ErrSendTransaction r
  = ErrNative + r

sendTransaction :: forall r. B.Web3 -> { from :: W3.Address, to :: W3.Address, value :: Number } -> ExceptV (ErrSendTransaction + r) Aff HexString
sendTransaction web3 opts =
  unsafePartial
    ( B.sendTransaction web3 { from: show opts.from, to: show opts.to, value: opts.value }
        # fromEffectFnAff
        <#> (mkHexString >>> fromJust)
        # attempt
        <#> lmap mkErrorNative
        # ExceptT
    )

--------------------------------------------------------------------------------
-- Web3
--------------------------------------------------------------------------------
type Result e a
  = ExceptV e Aff a

type ErrNewWebSocketProvider r
  = ErrNative + r

newWebSocketProvider :: forall r. URI -> ExceptV (ErrNewWebSocketProvider r) Effect B.Provider
newWebSocketProvider x1 =
  B.newWebSocketProvider (U.print x1)
    # try
    <#> lmap mkErrorNative
    # ExceptT

newWeb3 :: B.Provider -> Effect B.Web3
newWeb3 = B.newWeb3

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------
mkErrorNative :: forall r. Error -> Variant (ErrNative + r)
mkErrorNative e = _errNative { message: message e, name: name e }
