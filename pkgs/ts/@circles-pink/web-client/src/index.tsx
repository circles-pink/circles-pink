import { _StateMachine } from '@circles-pink/state-machine/src';
import React from 'react';
import { Onboarding, OnboardingProps } from './onboarding';

const Landing = (props: OnboardingProps) => {
  return <Onboarding {...props} content={{}} initState={_StateMachine.initLanding} />;
};

export default Landing;
