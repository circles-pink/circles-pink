import { ReactElement } from 'react';

type DefsOf<P, NP extends Partial<Required<P>>> = {
  [K in keyof P as undefined extends P[K]
    ? undefined extends NP[K]
      ? never
      : K
    : never]-?: NP[K];
};

export const withDefaults = <P, NP extends Partial<Required<P>>>(
  props: P,
  defaults: DefsOf<P, NP>
): NP => ({ ...defaults, ...props } as unknown as NP);

export const normalizeUI =
  <P, NP>(
    normalizeProps: (props: P) => NP,
    ui: (normProps: NP) => ReactElement
  ) =>
  (props: P): ReactElement =>
    ui(normalizeProps(props));
