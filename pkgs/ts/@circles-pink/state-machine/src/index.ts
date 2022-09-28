// import * as Data_Array from "@circles-pink/state-machine/output/Data.Array";
import * as Simple_Data_Array from "@circles-pink/state-machine/output/Simple.Data.Array";
import * as Data_Either from "@circles-pink/state-machine/output/Data.Either";
import * as Simple_Data_Either from "@circles-pink/state-machine/output/Simple.Data.Either";
import * as CirclesSimple_Data_Graph from "@circles-pink/state-machine/output/CirclesSimple.Data.Graph";
import * as CirclesSimple_Data_Graph_Diff from "@circles-pink/state-machine/output/CirclesSimple.Data.Graph.Diff";
import * as Data_Nullable from "@circles-pink/state-machine/output/Data.Nullable";
import * as Data_Maybe from "@circles-pink/state-machine/output/Data.Maybe";
import * as Data_Int from "@circles-pink/state-machine/output/Data.Int";
import * as Data_Tuple from "@circles-pink/state-machine/output/Data.Tuple";
import * as Simple_Data_Tuple from "@circles-pink/state-machine/output/Simple.Data.Tuple";
import * as Data_DateTime_Instant from "@circles-pink/state-machine/output/Data.DateTime.Instant";
import * as Data_Time_Duration from "@circles-pink/state-machine/output/Data.Time.Duration";
import * as Simple_Data_Duration from "@circles-pink/state-machine/output/Simple.Data.Duration";
import * as Simple_Network_Ethereum_Core_Signatures from "@circles-pink/state-machine/output/Simple.Network.Ethereum.Core.Signatures";
import * as Network_Ethereum_Core_Signatures from "@circles-pink/state-machine/output/Network.Ethereum.Core.Signatures";
import * as Simple_Data_Maybe from "@circles-pink/state-machine/output/Simple.Data.Maybe";
import * as CirclesPink_Data_UserIdent from "@circles-pink/state-machine/output/CirclesPink.Data.UserIdent";
import * as Data_Pair from "@circles-pink/state-machine/output/Data.Pair";
import * as Simple_Data_Pair from "@circles-pink/state-machine/output/Simple.Data.Pair";
import * as CirclesPink_Data_Address from "@circles-pink/state-machine/output/CirclesPink.Data.Address";
import * as CirclesPink_Data_Mnemonic from "@circles-pink/state-machine/output/CirclesPink.Data.Mnemonic";
import * as RemoteData from "@circles-pink/state-machine/output/RemoteData";
import * as Data_IxGraph from "@circles-pink/state-machine/output/Data.IxGraph";
import * as CirclesPink_Data_TrustState from "@circles-pink/state-machine/output/CirclesPink.Data.TrustState";
import * as CirclesPink_Data_TrustConnection from "@circles-pink/state-machine/output/CirclesPink.Data.TrustConnection";
import * as CirclesPink_Data_TrustNode from "@circles-pink/state-machine/output/CirclesPink.Data.TrustNode";
import * as CirclesPink_Garden_StateMachine_Direction from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Direction";
import * as CirclesPink_Garden_StateMachine_State from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State";
import * as CirclesPink_Garden_StateMachine_Action from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action";
import * as CirclesPink_Garden_StateMachine_TrackingEvent from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.TrackingEvent";
import * as CirclesPink_Garden_StateMachine_TrackingResumee from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.TrackingResumee";
import * as CirclesPink_TS from "@circles-pink/state-machine/output/CirclesPink.Garden.TS";
import * as Data_Unit from "@circles-pink/state-machine/output/Data.Unit";
import * as RemoteReport from "@circles-pink/state-machine/output/RemoteReport";
import * as VoucherServer from "@circles-pink/state-machine/output/VoucherServer.Spec.Types";

// -----------------------------------------------------------------------------
// Type exports
// -----------------------------------------------------------------------------

export type { Effect } from "@circles-pink/state-machine/output/Effect";
export type { Unit } from "@circles-pink/state-machine/output/Data.Unit";
export type { Maybe } from "@circles-pink/state-machine/output/Data.Maybe";
export type { Either } from "@circles-pink/state-machine/output/Data.Either";
export type { User } from "@circles-pink/state-machine/output/CirclesPink.Data.User";
export type { Address } from "@circles-pink/state-machine/output/CirclesPink.Data.Address";
export type { Address as EthAddress } from "@circles-pink/state-machine/output/Network.Ethereum.Core.Signatures";
export type { PrivateKey } from "@circles-pink/state-machine/output/CirclesPink.Data.PrivateKey.Type";
export type { Mnemonic } from "@circles-pink/state-machine/output/CirclesPink.Data.Mnemonic";
export type { UserIdent } from "@circles-pink/state-machine/output/CirclesPink.Data.UserIdent";
export type { IxGraph } from "@circles-pink/state-machine/output/Data.IxGraph";
export type { TrustState as TrustStateType } from "@circles-pink/state-machine/output/CirclesPink.Data.TrustState";
export type { TrustConnection } from "@circles-pink/state-machine/output/CirclesPink.Data.TrustConnection";
export type { TrustNode } from "@circles-pink/state-machine/output/CirclesPink.Data.TrustNode";
export type { TrackingEvent } from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.TrackingEvent";
export type { RemoteData } from "@circles-pink/state-machine/output/RemoteData";
export type { Direction } from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Direction";
export type { FetchImpl } from "@circles-pink/state-machine/output/Milkis.Impl";
export type { Object as ForeignObject } from "@circles-pink/state-machine/output/Foreign.Object";
export type {
  CirclesState,
  DebugState,
  LandingState,
  LoginState,
  UserData,
  TrustState,
  DashboardState,
} from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State";
export type {
  CirclesAction,
  DashboardAction,
  DebugAction,
} from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action";
export type {
  Voucher,
  VoucherAmount,
  VoucherProviderId,
  VoucherProvider,
  VoucherOffer,
} from "@circles-pink/state-machine/output/VoucherServer.Spec.Types";
export type { Pair } from "@circles-pink/state-machine/output/Data.Pair";
export type {
  CirclesGraph,
  VouchersResult,
  VoucherProvidersResult,
} from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard";
export type { CirclesConfigEffect } from "@circles-pink/state-machine/output/CirclesPink.Garden.TS";
export type { Resumee } from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.TrackingResumee";
export type { Json } from "@circles-pink/state-machine/output/Data.Argonaut";
export type { Instant } from "@circles-pink/state-machine/output/Data.DateTime.Instant";
export type { Int } from "@circles-pink/state-machine/output/PursTsGen.Prim";

// -----------------------------------------------------------------------------
// Value exports
// -----------------------------------------------------------------------------

export { unTrustState } from "@circles-pink/state-machine/output/CirclesPink.Data.TrustState";
export { mapArray } from "@circles-pink/state-machine/output/Simple.Data.Array";
export { unit } from "@circles-pink/state-machine/output/Data.Unit";

// export const _Array = Data_Array;
export const _Array = { ...Simple_Data_Array };
export const _Tuple = { ...Data_Tuple, ...Simple_Data_Tuple };
export const _Maybe = { ...Data_Maybe, ...Simple_Data_Maybe };
export const _UserIdent = CirclesPink_Data_UserIdent;
export const _Pair = { ...Data_Pair, ...Simple_Data_Pair };
export const _Address = CirclesPink_Data_Address;
export const _EthAddress = {
  ...Simple_Network_Ethereum_Core_Signatures,
  ...Network_Ethereum_Core_Signatures,
};
export const _Mnemonic = CirclesPink_Data_Mnemonic;
export const _RemoteData = RemoteData;
export const _IxGraph = Data_IxGraph;
export const _Graph = {
  ...CirclesSimple_Data_Graph,
  ...CirclesSimple_Data_Graph_Diff,
};
export const _TrustState = CirclesPink_Data_TrustState;
export const _Either = { ...Data_Either, ...Simple_Data_Either };
export const _Nullable = Data_Nullable;
export const _TrustConnection = CirclesPink_Data_TrustConnection;
export const _TrustNode = CirclesPink_Data_TrustNode;
export const _Unit = Data_Unit;
export const _Direction = CirclesPink_Garden_StateMachine_Direction;
export const _RemoteReport = RemoteReport;
export const _VoucherServer = VoucherServer;
export const _TrackingEvent = CirclesPink_Garden_StateMachine_TrackingEvent;
export const _StateMachine = {
  ...CirclesPink_Garden_StateMachine_State,
  ...CirclesPink_Garden_StateMachine_Action,
};
export const _TrackingResumee = CirclesPink_Garden_StateMachine_TrackingResumee;
export const _TS = CirclesPink_TS;
export const _Instant = Data_DateTime_Instant;
export const _Duration = { ...Data_Time_Duration, ...Simple_Data_Duration };
export const _Int = Data_Int;
