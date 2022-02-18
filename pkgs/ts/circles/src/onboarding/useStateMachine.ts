import { useState } from 'react';
import { Unit } from 'generated/output/Data.Unit';
import { unit } from 'generated/output/Data.Unit';
import { Effect } from 'generated/output/Effect';

export type StateMachine<S, M> = (setState: (st: S) => Effect<Unit>) => (s: S) => (msg: M) => Effect<Unit>

export const useStateMachine = <S, M>(initState: S, stateMachine: StateMachine<S, M>): [S, (m: M) => void] => {
    const [state, setState] = useState(initState);

    const act = (m: M) => {
        stateMachine(
            (s) => () => {
                setState(s)
                return unit
            }
        )(state)(m)()
    }

    return [state, act]
}
