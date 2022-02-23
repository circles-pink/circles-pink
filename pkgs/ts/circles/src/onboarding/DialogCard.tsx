import React, { ReactElement } from 'react';
import tw from 'twin.macro';

import { CirclesAction as Msg } from 'generated/output/CirclesPink.StateMachine.Action';

import { CirclesState as StateOnboard } from 'generated/output/CirclesPink.StateMachine.State';



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
  children?: ReactElement
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

export const DialogCard = ({ title, sub, username, email, next, back, children }: DemoCardProps): ReactElement => {
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
          {children}
        </Content>
        <Control>
          {back && <PrevButton onClick={() => back.act(back.msg)}>Back</PrevButton>}
          {next && <NextButton onClick={() => next.act(next.msg)}>Next</NextButton>}
        </Control>
      </Card>
    </Frame>
  )
};