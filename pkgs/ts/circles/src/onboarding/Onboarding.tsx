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
          debug={JSON.stringify(state.value.usernameApiResult, null, 2)}
          setUsername={(n: string) => act(A._askUsername(A._setUsername(n)))}
          back={() => act(A._askUsername(A._prev(unit)))}
          // next={() => act(A._askUsername(A._next(unit)))}
        />
      );
  }
};

// export const Onboarding = (): ReactElement => {
//   const [state, act] = useStateMachine(StateOnboard.init, myStateMachine);

//   switch (state.constructor.name as StateOnboard.State["$$pursTag"]) {
//     case "InfoGeneral": return (
//       <DialogCard
//         title="Welcome to Circles"
//         sub="Let's get you a circles Wallet!"
//         next={{
//           act,
//           msg: new StateOnboard.Next,
//           label: "Next"
//         }}
//       />
//     )
//     case "AskUsername": return (
//       <DialogCard
//         title="Tell me your Name!"
//         sub="Choose wisely, you can not change it later!"
//         username={{
//           act,
//           placeholder: 'Your amazing username',
//           value: (state as Extract<StateOnboard.State, { "$$pursTag": "AskUsername" }>).value0.username
//         }}
//         next={{
//           act,
//           msg: new StateOnboard.Next,
//           label: "Next"
//         }}
//         back={{
//           act,
//           msg: new StateOnboard.Prev,
//           label: "Back"
//         }}
//       />
//     )
//     case "AskEmail": return (
//       <DialogCard
//         title="How can I contact you?"
//         sub="Please submit a valid e-mail address."
//         email={{
//           act,
//           placeholder: 'Enter your E-Mail',
//           value: (state as Extract<StateOnboard.State, { "$$pursTag": "AskEmail" }>).value0.email
//         }}
//         next={{
//           act,
//           msg: new StateOnboard.Next,
//           label: "Next"
//         }}
//         back={{
//           act,
//           msg: new StateOnboard.Prev,
//           label: "Back"
//         }}
//       />
//     )
//     default: return (
//       <DialogCard
//         title="Ooops, state not found."
//         sub="Go back an try again! :/"
//         back={{
//           act,
//           msg: new StateOnboard.Prev,
//           label: "Back"
//         }}
//       />
//     )
//   }
// }
