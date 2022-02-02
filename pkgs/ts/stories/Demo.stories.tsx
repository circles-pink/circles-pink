import React from 'react';

import * as StateOnboardTS from "../../../output/Core.State.Onboard.TS"

import * as StateOnboard from "../../../output/Core.State.Onboard"


const x = StateOnboardTS.reducerPromise({
    apiCheckEmail: (e: string) => 1 as unknown as Promise<boolean>,
    apiCheckUserName: (u: string) => 1 as unknown as Promise<boolean>
})(StateOnboard.SetUsername.create("hello"))(StateOnboard.init)

import { ComponentMeta } from '@storybook/react';

const Button = ({ }) => <button>Hello</button>

export default {
    title: 'Demo',
    component: Button,
} as ComponentMeta<typeof Button>;


export const Primary = () => <Button />;
