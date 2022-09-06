// import * as Data_Array from "@circles-pink/state-machine/output/Data.Array";
import * as Simple_Data_Array from "@circles-pink/state-machine/output/Simple.Data.Array";
import * as Data_Either from "@circles-pink/state-machine/output/Data.Either";
import * as Data_Nullable from "@circles-pink/state-machine/output/Data.Nullable";
import * as Data_Maybe from "@circles-pink/state-machine/output/Data.Maybe";
import * as Data_Tuple from "@circles-pink/state-machine/output/Data.Tuple";
import * as Simple_Data_Tuple from "@circles-pink/state-machine/output/Simple.Data.Tuple";
import * as Simple_Data_Maybe from "@circles-pink/state-machine/output/Simple.Data.Maybe";
import * as CirclesPink_Data_UserIdent from "@circles-pink/state-machine/output/CirclesPink.Data.UserIdent";
import * as Data_Pair from "@circles-pink/state-machine/output/Data.Pair";
import * as CirclesPink_Data_Address from "@circles-pink/state-machine/output/CirclesPink.Data.Address";
import * as RemoteData from "@circles-pink/state-machine/output/RemoteData";
import * as Data_IxGraph from "@circles-pink/state-machine/output/Data.IxGraph";
import * as CirclesPink_Data_TrustState from "@circles-pink/state-machine/output/CirclesPink.Data.TrustState";
import * as CirclesPink_Data_TrustConnection from "@circles-pink/state-machine/output/CirclesPink.Data.TrustConnection";

export type { Maybe } from "@circles-pink/state-machine/output/Data.Maybe";
export type { Address } from "@circles-pink/state-machine/output/CirclesPink.Data.Address";
export type { UserIdent } from "@circles-pink/state-machine/output/CirclesPink.Data.UserIdent";
export type { IxGraph } from "@circles-pink/state-machine/output/Data.IxGraph";
export type { TrustState } from "@circles-pink/state-machine/output/CirclesPink.Data.TrustState";
export type { TrustConnection } from "@circles-pink/state-machine/output/CirclesPink.Data.TrustConnection";
export { unTrustState } from "@circles-pink/state-machine/output/CirclesPink.Data.TrustState";
export { Pair } from "@circles-pink/state-machine/output/Data.Pair";
export { mapArray } from "@circles-pink/state-machine/output/Simple.Data.Array";

export type {
  DashboardState,
  CirclesGraph,
} from "@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard";

// export const _Array = Data_Array;
export const _Array = { ...Simple_Data_Array };
export const _Tuple = { ...Data_Tuple, ...Simple_Data_Tuple };
export const _Maybe = { ...Data_Maybe, ...Simple_Data_Maybe };
export const _UserIdent = CirclesPink_Data_UserIdent;
export const _Pair = Data_Pair;
export const _Address = CirclesPink_Data_Address;
export const _RemoteData = RemoteData;
export const _IxGraph = Data_IxGraph;
export const _TrustState = CirclesPink_Data_TrustState;
export const _Either = Data_Either;
export const _Nullable = Data_Nullable;
export const _TrustConnection = CirclesPink_Data_TrustConnection;
