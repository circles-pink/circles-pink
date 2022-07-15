module CirclesPink.Data.TxState
  ( TxSeqState
  ) where

import CirclesPink.Prelude

import CirclesPink.Data.Address (Address)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BN (BN)
import Debug.Extra (todo)
import RemoteReport (RemoteReport)

type EitherV r a = Either (Variant r) a

newtype TxHash = TxHash String

newtype TxHop = TxHop { from :: Address, to :: Address, value :: BN, index :: Int }

data SucceededTxHop = SucceededTxHop TxHash TxHop



data TxSeqState
  = NoTxSeq
  | PrepareTxSeq (NonEmptyArray TxHop)
  | RunningTxSeq (NonEmptyArray TxHop) (RemoteReport (Variant (Err ())) TxHash)
type TxHopRemoteReport = RemoteReport (Variant (Err ())) TxHash

init :: forall r. TxSeqState -> EitherV (Err r) TxSeqState
init = todo

prepareTxSeq :: forall r. NonEmptyArray TxHop -> TxSeqState -> EitherV (Err r) TxSeqState
prepareTxSeq = todo

startTxSeq :: forall r. TxSeqState -> EitherV (Err r) TxSeqState
startTxSeq = todo

updateTxHop :: forall r. TxHopRemoteReport -> TxSeqState -> EitherV (Err r) TxSeqState
updateTxHop = todo

failTxSeq :: forall r. TxSeqState -> EitherV (Err r) TxSeqState
failTxSeq = todo

type Err :: forall k. k -> k
type Err r = r