// -----------------------------------------------------------------------------
// fields
// -----------------------------------------------------------------------------

import { pipe } from 'fp-ts/lib/function';

export const fields = <V>(d: V): ValuesToFields<V> => {
  var i = 0;
  const out = [];
  while (`value${i}` in d) {
    out.push((d as any)[`value${i}`]);
    i++;
  }
  return out as any;
};

type Prev = [never, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
type Idx = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];

type ValuesToFields<T, D extends Prev[number] = 9> = T extends {
  [key in `value${Idx[D]}`]: unknown;
}
  ? [T[`value${Idx[D]}`], ...ValuesToFields<T, Prev[D]>]
  : [];

// type ValuesToFields<T> = T extends { value6: infer A }
//   ? [...(T extends any ?  ValuesToFields<T> : never), A]
//   : [];

type O = ValuesToFields<{ value0: 6; value1: 2; value2: 3 }>;

const fields_ = <V>(values: V): V extends any ? ValuesToFields<V> : never => {
  return 1 as any;
};

// -----------------------------------------------------------------------------
// matchV
// -----------------------------------------------------------------------------

type Variant = { type: string; value: unknown };

type VariantCasesOf<V extends Variant, Z> = {
  [key in V['type']]: (x: Extract<V, { type: key }>['value']) => Z;
};

export const matchV =
  <V extends Variant, Z>(v: V) =>
  <Z>(c: VariantCasesOf<V, Z>): Z =>
    (c as any)[v.type](v.value);

// -----------------------------------------------------------------------------
// matchData
// -----------------------------------------------------------------------------

type ADT = { constructor: { name: string } };

type ADTCasesOf<D extends ADT, Z> = {
  [key in D['constructor']['name']]: (
    x: ValuesToFields<Extract<D, { constructor: { name: key } }>>
  ) => Z;
};

export const matchADT =
  <D extends ADT>(adt: D,) =>
  <Z>(c: ADTCasesOf<D, Z>): Z =>
    (c as any)[adt.constructor.name](fields(adt));
