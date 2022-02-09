import React, { ReactElement } from 'react';
import { ComponentMeta } from '@storybook/react';
import { DemoComponent } from 'circles/src/bar';
import { pi } from "circles/src/foo"

const Button = ({ }): ReactElement => {
  return <DemoComponent />
}

export default {
  title: 'Components/Demo',
  component: Button,
} as ComponentMeta<typeof Button>;


export const Primary = () => <Button />;
