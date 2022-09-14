import { useState } from 'react';
import { Effect, Unit, unit } from '@circles-pink/state-machine/src';

export type StateMachine<S, M> = (
  setState: (st: (orig: S) => S) => Effect<Unit>
) => (s: S) => (msg: M) => Effect<Unit>;

export const useStateMachine = <S, M>(
  initState: S,
  stateMachine: StateMachine<S, M>
): [S, (m: M) => void] => {
  const [state, setState] = useState(initState);

  const act = (m: M) => {
    stateMachine(s => () => {
      setState(s);
      return unit;
    })(state)(m)();
  };

  return [state, act];
};
