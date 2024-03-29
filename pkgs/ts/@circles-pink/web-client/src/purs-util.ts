// -----------------------------------------------------------------------------
// fields
// -----------------------------------------------------------------------------

import { unRemoteData } from "@circles-pink/state-machine/output/RemoteData";
import { ReadonlyRecord } from "fp-ts/lib/ReadonlyRecord";

// export const fieldsOf =
//   <L extends string>(l: L) =>
//   <V extends { constructor: { name: L } }>(d: V): ValuesToFields<V> => {
//     var i = 0;
//     const out = [];
//     while (`value${i}` in d) {
//       out.push((d as any)[`value${i}`]);
//       i++;
//     }
//     return out as any;
//   };

// type Prev = [never, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
// type Idx = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];

// type ValuesToFields<T, D extends Prev[number] = 9> = T extends {
//   [key in `value${Idx[D]}`]: unknown;
// }
//   ? [T[`value${Idx[D]}`], ...ValuesToFields<T, Prev[D]>]
//   : [];

// export const fieldOf =
//   <L extends string>(l: L) =>
//   <T>(x: { value0: T; constructor: { name: L } }): T =>
//     x.value0;

// -----------------------------------------------------------------------------
// matchV
// -----------------------------------------------------------------------------

type Variant<T = string, V = unknown> = { type: T; value: V };

type VariantCasesOf<V extends Variant, Z> = {
  [key in V['type']]: (x: Extract<V, { type: key }>['value']) => Z;
};

type VariantCasesOf_<
  V extends Variant,
  Z,
  D extends undefined | (() => Z)
> = Extract<D, undefined> extends never
  ? Partial<VariantCasesOf<V, Z>>
  : VariantCasesOf<V, Z>;

export const matchV =
  <V extends Variant, Z>(v: V) =>
  <Z, D extends undefined | (() => Z)>(
    c: VariantCasesOf_<V, Z, D>,
    defCase?: D
  ): Z =>
    ((c as any)[v.type] || defCase)(v.value);

// -----------------------------------------------------------------------------
// isV
// -----------------------------------------------------------------------------

export const isCaseV =
  <L extends V['type'], V extends Variant>(s: L) =>
  (v: V): v is Extract<V, { type: L }> =>
    s === v.type;

// -----------------------------------------------------------------------------
// matchData
// -----------------------------------------------------------------------------

// type ADT = { constructor: { name: string } };

// type ADTCasesOf<D extends ADT, Z> = {
//   [key in D['constructor']['name']]: (
//     x: ValuesToFields<Extract<D, { constructor: { name: key } }>>
//   ) => Z;
// };

// export const matchADT =
//   <D extends ADT>(adt: D) =>
//   <Z>(c: ADTCasesOf<D, Z>): Z =>
//     (c as any)[adt.constructor.name](fieldsOf(adt as any));

// -----------------------------------------------------------------------------
// is
// -----------------------------------------------------------------------------

// export const isCase =
//   <L extends string>(s: L) =>
//   <A extends ADT>(a: A): a is Extract<A, { constructor: { name: L } }> =>
//     s === a.constructor.name;

// export const run = <Z>(f: () => Z): Z => f();

// -----------------------------------------------------------------------------
// withDefault
// -----------------------------------------------------------------------------


// type CaseFn<Z> = 
//  // | ((x1: any) => (x2: any) => (x3: any) => Z)
//  // | ((x1: any) => (x2: any) => Z)
//   | ((x1: any) => Z)

// type CaseFns<K extends (string), Z> = ReadonlyRecord<K, CaseFn<Z>>

// //type UnFn <A, Z> = (_: CaseFns<Z>) => any // <A>(x: A) => Z

// const withDefault = <U extends ReadonlyRecord<keyof U,  CaseFn<any>>>(unFn : U) : any => 1 as any

// const x = withDefault(unRemoteData)


// ({
//   onSuccess: () => 3,
//   default: (): 0
// }, 0)