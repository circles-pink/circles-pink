import React, { ReactElement } from 'react';
import { ComponentMeta } from '@storybook/react';
import { Onboarding } from 'circles/src/Onboarding';


export default {
  title: 'Components/Onboarding',
  component: Onboarding,
} as ComponentMeta<typeof Onboarding>;


export const GardenAPI = (): ReactElement => <Onboarding />;
