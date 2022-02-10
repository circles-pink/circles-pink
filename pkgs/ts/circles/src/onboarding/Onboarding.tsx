import '../style/global.css';

import React, { ReactElement } from 'react';
import { DialogCard } from './DialogCard';

import * as GardenEnv from 'generated/output/Garden.Env';
import { milkisRequest } from 'generated/output/HTTP.Milkis';
import { windowFetch } from 'generated/output/Milkis.Impl.Window';

import * as StateOnboardTS from "generated/output/Core.State.Onboard.TS";
import { StateMachine, useStateMachine } from './useStateMachine';
import * as StateOnboard from "generated/output/Core.State.Onboard";

const myStateMachine: StateMachine<StateOnboard.State, StateOnboard.Msg>
  = StateOnboardTS.reducerAff(GardenEnv.env({
    request: milkisRequest(windowFetch)
  }))

export const Onboarding = (): ReactElement => {
  const [state, act] = useStateMachine(StateOnboard.init, myStateMachine);

  switch (state.constructor.name as StateOnboard.State["$$pursTag"]) {
    case "InfoGeneral": return (
      <DialogCard
        title="Welcome to Circles"
        sub="Let's get you a circles Wallet!"
        next={{
          act,
          msg: new StateOnboard.Next,
          label: "Next"
        }}
      />
    )
    case "AskUsername": return (
      <DialogCard
        title="Tell me your Name!"
        sub="Choose wisely, you can not change it later!"
        username={{
          act,
          placeholder: 'Your amazing username',
          value: (state as Extract<StateOnboard.State, { "$$pursTag": "AskUsername" }>).value0.username
        }}
        next={{
          act,
          msg: new StateOnboard.Next,
          label: "Next"
        }}
        back={{
          act,
          msg: new StateOnboard.Prev,
          label: "Back"
        }}
      />
    )
    case "AskEmail": return (
      <DialogCard
        title="How can I contact you?"
        sub="Please submit a valid e-mail address."
        email={{
          act,
          placeholder: 'Enter your E-Mail',
          value: (state as Extract<StateOnboard.State, { "$$pursTag": "AskEmail" }>).value0.email
        }}
        next={{
          act,
          msg: new StateOnboard.Next,
          label: "Next"
        }}
        back={{
          act,
          msg: new StateOnboard.Prev,
          label: "Back"
        }}
      />
    )
    default: return (
      <DialogCard
        title="Ooops, state not found."
        sub="Go back an try again! :/"
        back={{
          act,
          msg: new StateOnboard.Prev,
          label: "Back"
        }}
      />
    )
  }
}