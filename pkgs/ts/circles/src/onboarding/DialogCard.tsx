import React, { ReactElement } from 'react';
import tw from 'twin.macro';

import { Msg } from 'generated/output/Core.State.Onboard';
import * as StateOnboard from "generated/output/Core.State.Onboard";

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

const Frame = tw.div`bg-gray-50 border-2 border-pink-500`;
const Card = tw.div`max-w-7xl mx-auto py-12 px-4 sm:px-6 lg:py-16 lg:px-8 lg:flex lg:items-center lg:justify-between`;
const Content = tw.div``;
const Text = tw.h2`text-3xl font-extrabold tracking-tight text-gray-900 sm:text-4xl mb-2`;
const Claim = tw.span`block`;
const SubClaim = tw.span`block text-pink-600`;
const Control = tw.div`mt-8 flex lg:mt-0 lg:flex-shrink-0`;
const NextButton = tw.button`bg-pink-600 hover:bg-pink-700 text-white font-bold py-2 px-4 rounded-full mr-1`;
const PrevButton = tw.button`bg-gray-600 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded-full mr-1`;

export const DialogCard = ({ title, sub, username, email, next, back }: DemoCardProps): ReactElement => {
  const inputStyle = 'shadow appearance-none border border-pink-600 rounded w-full py-2 px-3 text-gray-700 mb-3 leading-tight w-full'
  const AskUsername = tw.input`${inputStyle}`;
  const AskEmail = tw.input`${inputStyle}`;

  return (
    <Frame>
      <Card>
        <Content>
          <Text>
            <Claim>{title}</Claim>
            <SubClaim>{sub}</SubClaim>
          </Text>
          {username && <AskUsername type="text"
            value={username.value}
            placeholder={username.placeholder}
            onChange={(e) => username.act(new StateOnboard.SetUsername(e.target.value))}
          />}
          {email && <AskEmail type="text"
            value={email.value}
            placeholder={email.placeholder}
            onChange={(e) => email.act(new StateOnboard.SetEmail(e.target.value))}
          />}
        </Content>
        <Control>
          {back && <PrevButton onClick={() => back.act(back.msg)}>Back</PrevButton>}
          {next && <NextButton onClick={() => next.act(next.msg)}>Next</NextButton>}
        </Control>
      </Card>
    </Frame>
  )
};