import React from 'react';
import * as StateOnboardTS from "../../../../generated/output/Core.State.Onboard.TS"
import * as StateOnboard from "../../../../generated/output/Core.State.Onboard"
import * as GardenEnv from '../../../../generated/output/Garden.Env'

import { ComponentMeta } from '@storybook/react';
import { Aff } from '../../../../generated/output/Effect.Aff';
import { Unit } from '../../../../output/Data.Unit';
import { Effect } from '../../../../output/Effect';
import { StateMachine, useStateMachine } from '../useStateMachine';

const myStateMachine: StateMachine<StateOnboard.State, StateOnboard.Msg>
    = StateOnboardTS.reducerAff(GardenEnv.env)


const Button = ({ }) => {
    const [state, act] = useStateMachine(StateOnboard.init, myStateMachine);

    switch (state.constructor.name as StateOnboard.State["$$pursTag"]) {
        case "InfoGeneral": return (
            <div>
                InfoGeneral
                <button onClick={() => act(new StateOnboard.Next)}>Next</button>
            </div>
        )
        case "AskUsername": return (
            <div>
                Ask UserName
                ....
            </div>
        )
        default: return (<div>????{state.$$pursType}</div>)
    }
}

export default {
    title: 'Demo',
    component: Button,
} as ComponentMeta<typeof Button>;


export const Primary = () => <Button />;
