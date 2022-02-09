import React, { ReactElement } from 'react';
import { ComponentMeta } from '@storybook/react';
import { DemoComponent } from 'circles/src';

const Button = ({ }): ReactElement => {
  return <DemoComponent />
}

export default {
  title: 'Components/DemoTwo',
  component: Button,
} as ComponentMeta<typeof Button>;


export const Primary = () => <Button />;
