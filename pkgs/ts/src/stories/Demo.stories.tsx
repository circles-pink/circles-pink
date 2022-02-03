import React from 'react';
import * as StateOnboardTS from "../../../../generated/output/Core.State.Onboard.TS"
import * as StateOnboard from "../../../../generated/output/Core.State.Onboard"
import * as GardenEnv from '../../../../generated/output/Garden.Env'

import { ComponentMeta } from '@storybook/react';

// const x = () => StateOnboardTS.reducerPromise({
//     apiCheckUserName: (u: string) => 1 as unknown as Promise<boolean>,
//     apiCheckEmail: (e: string) => 1 as unknown as Promise<boolean>,
// })((st) => 1 as any)(StateOnboard.SetUsername.create("hello"))(StateOnboard.init)

const x = StateOnboardTS.reducerAff(GardenEnv.env)

const Button = ({ }) => <button>Hello</button>

export default {
    title: 'Demo',
    component: Button,
} as ComponentMeta<typeof Button>;


export const Primary = () => <Button />;
