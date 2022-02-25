import '../style/global.css';

import React, { ReactElement } from 'react';

import * as GardenEnv from 'generated/output/Garden.Env';
import { milkisRequest } from 'generated/output/HTTP.Milkis';
import { windowFetch } from 'generated/output/Milkis.Impl.Window';
import { circlesControlEff } from 'generated/output/CirclesPink.TS';
import { init } from 'generated/output/CirclesPink.StateMachine.State';
import * as A from 'generated/output/CirclesPink.StateMachine.Action';
import { unit } from 'generated/output/Data.Unit';

import { useStateMachine } from './useStateMachine';
import { AskUsername } from './view/AskUsername';
import { InfoGeneral } from './view/InfoGeneral';
import { AskEmail } from './view/AskEmail';
import { InfoSecurity } from './view/InfoSecurity';
import { mapIndicatorColors } from './mapIndicatorColors';

const control = circlesControlEff(
  GardenEnv.env({
    request: milkisRequest(windowFetch),
  })
);

export const Onboarding = (): ReactElement => {
  const [state, act] = useStateMachine(init, control);

  switch (state.type) {
    case 'infoGeneral':
      return <InfoGeneral next={() => act(A._infoGeneral(A._next(unit)))} />;
    case 'askUsername':
      return (
        <AskUsername
          username={state.value.username}
          setUsername={(n: string) => act(A._askUsername(A._setUsername(n)))}
          indicatorColor={mapIndicatorColors(state.value.usernameApiResult)}
          back={() => act(A._askUsername(A._prev(unit)))}
          next={() => act(A._askUsername(A._next(unit)))}
          debug={JSON.stringify(state.value.usernameApiResult, null, 2)}
        />
      );
    case 'askEmail':
      return (
        <AskEmail
          email={state.value.email}
          terms={state.value.terms}
          privacy={state.value.privacy}
          indicatorColor={mapIndicatorColors(state.value.emailApiResult)}
          setEmail={(n: string) => act(A._askEmail(A._setEmail(n)))}
          setTerms={() => act(A._askEmail(A._setTerms(unit)))}
          setPrivacy={() => act(A._askEmail(A._setPrivacy(unit)))}
          back={() => act(A._askEmail(A._prev(unit)))}
          next={() => act(A._askEmail(A._next(unit)))}
          debug={JSON.stringify(state.value.emailApiResult, null, 2)}
        />
      );
    case 'infoSecurity':
      return <InfoSecurity back={() => act(A._infoSecurity(A._prev(unit)))} />;
    default:
      return <h2>Invalid State</h2>;
  }
};
