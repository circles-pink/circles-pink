import React from 'react';

import { ComponentMeta } from '@storybook/react';

const Button = ({ }) => <button>Hello</button>

export default {
    title: 'Demo',
    component: Button,
} as ComponentMeta<typeof Button>;


export const Primary = () => <Button />;
