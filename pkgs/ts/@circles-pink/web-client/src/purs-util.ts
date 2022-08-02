export function fields<A0, A1, A2, A3, A4, A5, A6>(d: {
  value0: A0;
  value1: A1;
  value2: A2;
  value3: A3;
  value4: A4;
  value5: A5;
  value6: A6;
}): readonly [A0, A1, A2, A3, A4, A5, A6];

export function fields<A0, A1, A2, A3, A4, A5>(d: {
  value0: A0;
  value1: A1;
  value2: A2;
  value3: A3;
  value4: A4;
  value5: A5;
}): readonly [A0, A1, A2, A3, A4, A5];

export function fields<A0, A1, A2, A3, A4>(d: {
  value0: A0;
  value1: A1;
  value2: A2;
  value3: A3;
  value4: A4;
}): readonly [A0, A1, A2, A3, A4];

export function fields<A0, A1, A2, A3>(d: {
  value0: A0;
  value1: A1;
  value2: A2;
  value3: A3;
}): readonly [A0, A1, A2, A3];

export function fields<A0, A1, A2>(d: {
  value0: A0;
  value1: A1;
  value2: A2;
}): readonly [A0, A1, A2];

export function fields<A0, A1>(d: { value0: A0; value1: A1 }): [A0, A1];

export function fields<A0>(d: { value0: A0 }): [A0];

export function fields(d: {}): unknown {
  var i = 0;
  const out = [];
  while (`value${i}` in d) {
    out.push((d as any)[`value${i}`]);
    i++;
  }
  return out;
}
