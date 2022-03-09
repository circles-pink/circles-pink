import '../styles/global.css';

import React, { ReactElement } from 'react';

import { control } from 'generated/output/CirclesPink.Garden.TS';
import {
  CirclesState,
  init,
} from 'generated/output/CirclesPink.Garden.StateMachine.State';

import { useStateMachine } from './utils/useStateMachine';
import {
  AskUsername,
  InfoGeneral,
  AskEmail,
  InfoSecurity,
  MagicWords,
} from './views';

type OnboardingProps = {
  initState?: CirclesState;
};

export const Onboarding = ({ initState }: OnboardingProps): ReactElement => {
  const [state, act] = useStateMachine(initState || init, control);

  switch (state.type) {
    case 'infoGeneral':
      return <InfoGeneral state={state.value} act={act} />;
    case 'askUsername':
      return <AskUsername state={state.value} act={act} />;
    case 'askEmail':
      return <AskEmail state={state.value} act={act} />;
    case 'infoSecurity':
      return <InfoSecurity state={state.value} act={act} />;
    case 'magicWords':
      return <MagicWords state={state.value} act={act} />;
    default:
      return <h2>Invalid State</h2>;
  }
};
