import { initLanding } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import React from 'react';
import { Onboarding, OnboardingProps } from './onboarding';

const Landing = (props: OnboardingProps) => {
  return <Onboarding {...props} content={{}} initState={initLanding} />;
};

export default Landing;
