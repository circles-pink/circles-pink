import React, { ReactElement } from 'react';

import * as StateOnboardTS from "../../../../../../generated/output/Core.State.Onboard.TS"
import * as StateOnboard from "../../../../../../generated/output/Core.State.Onboard"
import * as GardenEnv from '../../../../../../generated/output/Garden.Env'
import { milkisRequest } from '../../../../../../generated/output/HTTP.Milkis'
import { windowFetch } from '../../../../../../generated/output/Milkis.Impl.Window'

import { ComponentMeta } from '@storybook/react';
import { StateMachine, useStateMachine } from '../../useStateMachine';
import { Msg } from '../../../../../../generated/output/Core.State.Onboard';

const myStateMachine: StateMachine<StateOnboard.State, StateOnboard.Msg>
  = StateOnboardTS.reducerAff(GardenEnv.env({
    request: milkisRequest(windowFetch)
  }))

const nextButton = "bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full mr-1"
const backButton = "bg-gray-600 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded-full mr-1"
const inputClass = "shadow appearance-none border border-pink-600 rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight focus:outline-none focus:shadow-outline"

const Button = ({ }) => {
  const [state, act] = useStateMachine(StateOnboard.init, myStateMachine);

  switch (state.constructor.name as StateOnboard.State["$$pursTag"]) {
    case "InfoGeneral": return (
      <DemoCard
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
      <DemoCard
        title="Tell me your Name!"
        sub="Choose wisely, you cannot change it later!"
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
      <DemoCard
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
      <DemoCard
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

export default {
  title: 'Components/Demo',
  component: Button,
} as ComponentMeta<typeof Button>;


export const Primary = () => <Button />;

type DemoCardProps = {
  title: string,
  sub: string,
  username?: {
    act: (m: Msg) => void,
    placeholder: string;
    value: string;
  },
  email?: {
    act: (m: Msg) => void,
    placeholder: string;
    value: string;
  },
  next?: {
    act: (m: Msg) => void,
    msg: Msg,
    label: string
  },
  back?: {
    act: (m: Msg) => void,
    msg: Msg,
    label: string
  },
}

const DemoCard = ({ title, sub, username, email, next, back }: DemoCardProps): ReactElement => {
  return (
    <div className="bg-gray-50 border-2 border-pink-500">
      <div className="max-w-7xl mx-auto py-12 px-4 sm:px-6 lg:py-16 lg:px-8 lg:flex lg:items-center lg:justify-between">
        <div>
          <h2 className="text-3xl font-extrabold tracking-tight text-gray-900 sm:text-4xl mb-2">
            <span className="block">{title}</span>
            <span className="block text-pink-600">{sub}</span>
          </h2>
          {username && <div>
            <input type="text"
              className={inputClass}
              value={username.value}
              placeholder={username.placeholder}
              onChange={(e) => username.act(new StateOnboard.SetUsername(e.target.value))}
            />
          </div>}
          {email && <div>
            <input type="text"
              className={inputClass}
              value={email.value}
              placeholder={email.placeholder}
              onChange={(e) => email.act(new StateOnboard.SetEmail(e.target.value))}
            />
          </div>}
        </div>
        <div className="mt-8 flex lg:mt-0 lg:flex-shrink-0">
          {back && <button className={backButton} onClick={() => back.act(back.msg)}>Back</button>}
          {next && <button className={nextButton} onClick={() => next.act(next.msg)}>Next</button>}
        </div>
      </div>
    </div>
  )
}