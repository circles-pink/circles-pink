import '../style/global.css';

import React, { ReactElement } from 'react';
import { DialogCard } from './DialogCard';

import * as GardenEnv from 'generated/output/Garden.Env';
import { milkisRequest } from 'generated/output/HTTP.Milkis';
import { windowFetch } from 'generated/output/Milkis.Impl.Window';
import { circlesControlEff } from 'generated/output/CirclesPink.TS';
import { init } from 'generated/output/CirclesPink.StateMachine.State';
import * as A from 'generated/output/CirclesPink.StateMachine.Action';


//import * as StateOnboardTS from "generated/output/Core.State.Onboard.TS";
import { StateMachine, useStateMachine } from './useStateMachine';
import { unit } from 'generated/output/Data.Unit';
import tw from 'twin.macro';
//import * as StateOnboard from "generated/output/Core.State.Onboard";

// const myStateMachine: StateMachine<StateOnboard.State, StateOnboard.Msg>
//   = StateOnboardTS.reducerAff(GardenEnv.env({
//     request: milkisRequest(windowFetch)
//   }))

const control = circlesControlEff(GardenEnv.env({
  request: milkisRequest(windowFetch)
}))

const inputStyle = 'shadow appearance-none border border-pink-600 rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight w-full'
const AskUsername = tw.input`${inputStyle}`;

export const Onboarding = () => {
  const [state, act] = useStateMachine(init, control);

  console.log(state);


  switch (state.type) {
    case "infoGeneral": return (

      <DialogCard
        title="Welcome to Circles"
        sub="Let's get you a circles Wallet!"
        next={{
          act,
          msg: A._infoGeneral(A._next(unit)),
          label: "Next"
        }}
      />

    )
    case "askUsername": return (
      <DialogCard
        title="Tell me your Name!"
        sub="Choose wisely, you can not change it later!"
        // next={{
        //   act,
        //   msg: A._askUsername(A._next(unit)),
        //   label: "Next"
        // }}
        back={{
          act,
          msg: A._askUsername(A._prev(unit)),
          label: "Back"
        }}
      >
        <AskUsername type="text"
          value={state.value.username}
          placeholder={'Your amazing username'}
          onChange={(e) => act(A._askUsername(A._setUsername(e.target.value)))}
        />
      </DialogCard>
    )
  }
}

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