import '../style/global.css';

import React, { ReactElement } from 'react';

import { control } from 'generated/output/CirclesPink.Garden.TS';
import { init } from 'generated/output/CirclesPink.Garden.StateMachine.State';

import { useStateMachine } from './useStateMachine';
import { AskUsername } from './view/AskUsername';
import { InfoGeneral } from './view/InfoGeneral';
import { AskEmail } from './view/AskEmail';
import { InfoSecurity } from './view/InfoSecurity';
import { MagicWords } from './view/MagicWords';

export const Onboarding = (): ReactElement => {
  const [state, act] = useStateMachine(init, control);

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
