import { initLanding } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.ProtocolDef.States.Landing';
import React from 'react';
import { Onboarding } from './onboarding';

const Landing = () => {
  return <Onboarding content={{}} initState={initLanding} />;
};

export default Landing;
