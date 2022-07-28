module Web3
  ( ErrNative
  , ErrPrivKeyToAccount
  , Hash(..)
  , Message(..)
  , NativeError
  , SignatureObj(..)
  , accountsHashMessage
  , accountsRecover
  , accountsSign
  , module Exp
  , newWeb3
  , newWeb3_
  , newWebSocketProvider
  , privKeyToAccount
  , sendTransaction
  )
  where

import Prelude

import CirclesPink.Data.Address (Address, parseAddress)
import CirclesPink.URI (URI)
import CirclesPink.URI as U
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Checked (ExceptV)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff, Error, attempt, message, try)
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Exception (name)
import Effect.Unsafe (unsafePerformEffect)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Core.Signatures (PrivateKey)
import Network.Ethereum.Core.Signatures as W3
import Partial.Unsafe (unsafePartial)
import Record as R
import Simple.JSON (class ReadForeign)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Web3.Bindings (Web3) as Exp
import Web3.Bindings as B

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Hash = Hash String

derive instance newtypeHash :: Newtype Hash _
derive newtype instance readForeignHash :: ReadForeign Hash
derive instance eqHash :: Eq Hash

newtype Message = Message String

derive instance newtypeMessage :: Newtype Message _
derive newtype instance readForeignMessage :: ReadForeign Message

newtype SignatureObj = SignatureObj
  { message :: Message
  , messageHash :: Hash
  , v :: String
  , r :: String
  , s :: String
  , signature :: String
  }

derive instance newtypeSignatureObj :: Newtype SignatureObj _
derive newtype instance readForeignSignatureObj :: ReadForeign SignatureObj

--------------------------------------------------------------------------------
-- Error types
--------------------------------------------------------------------------------
type NativeError =
  { message :: String
  , name :: String
  }

type ErrNative r = (errNative :: NativeError | r)

--------------------------------------------------------------------------------
-- Err constructors
--------------------------------------------------------------------------------
_errNative :: forall r. NativeError -> Variant (ErrNative r)
_errNative = inj (Proxy :: _ "errNative")

--------------------------------------------------------------------------------
-- Send Transaction
--------------------------------------------------------------------------------
type ErrSendTransaction r = ErrNative + r

sendTransaction :: forall r. B.Web3 -> { from :: Address, to :: Address, value :: String } -> ExceptV (ErrSendTransaction + r) Aff HexString
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
type Result e a = ExceptV e Aff a

type ErrNewWebSocketProvider r = ErrNative + r

newWebSocketProvider :: forall r. URI -> ExceptV (ErrNewWebSocketProvider r) Aff B.Provider
newWebSocketProvider x1 =
  B.newWebSocketProvider (U.print x1)
    # liftEffect
    # try
    <#> lmap mkErrorNative
    # ExceptT

newWeb3 :: B.Provider -> Aff B.Web3
newWeb3 = B.newWeb3 >>> liftEffect

newWeb3_ :: Aff B.Web3
newWeb3_ = B.newWeb3_ # liftEffect

--------------------------------------------------------------------------------
type ErrPrivKeyToAccount r = ErrNative + r

privKeyToAccount :: forall r. B.Web3 -> W3.PrivateKey -> ExceptV (ErrPrivKeyToAccount r) Aff B.Account
privKeyToAccount w3 pk =
  B.privKeyToAccount w3 (show pk)
    # liftEffect
    # try
    <#> lmap mkErrorNative
    # ExceptT

--------------------------------------------------------------------------------
-- Accounts
--------------------------------------------------------------------------------

accountsSign :: B.Web3 -> Message -> PrivateKey -> SignatureObj
accountsSign web3 (Message msg) pk =
  B.accountsSign web3 msg (show pk)
    # toSignatureObj

accountsRecover :: B.Web3 -> SignatureObj -> Maybe Address
accountsRecover web3 so = so
  # fromSignatureObj
  # B.accountsRecover web3
  # try
  # unsafePerformEffect
  # hush
  >>= parseAddress

accountsHashMessage :: B.Web3 -> Message -> Hash
accountsHashMessage web3 (Message msg) = B.accountsHashMessage web3 msg # Hash

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------
mkErrorNative :: forall r. Error -> Variant (ErrNative + r)
mkErrorNative e = _errNative { message: message e, name: name e }

toSignatureObj :: B.SignatureObj -> SignatureObj
toSignatureObj r = r
  # R.modify (Proxy :: _ "message") Message
  # R.modify (Proxy :: _ "messageHash") Hash
  # SignatureObj

fromSignatureObj :: SignatureObj -> B.SignatureObj
fromSignatureObj r = r
  # unwrap
  # R.modify (Proxy :: _ "message") (un Message)
  # R.modify (Proxy :: _ "messageHash") (un Hash)