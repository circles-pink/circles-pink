import React, { ReactElement } from 'react';
import tw from 'twin.macro';

type DemoCardProps = {
  text: ReactElement;
  interaction?: ReactElement;
  control: ReactElement;
  debug?: ReactElement;
};

const Frame = tw.div`bg-gray-50 border border-pink-500 border-dotted`;
const Card = tw.div`max-w-7xl mx-auto py-12 px-4 sm:px-6 lg:py-16 lg:px-8 lg:flex lg:justify-between`;
const Content = tw.div`lg:w-1/2`;
const Control = tw.div`mt-8 flex lg:mt-0 lg:flex-shrink-0 lg:mt-8`;
const Debug = tw.div`p-8`;

export const DialogCard = ({
  text,
  interaction,
  control,
  debug,
}: DemoCardProps): ReactElement => {
  return (
    <>
      <Frame>
        <Card>
          <Content>
            {text}
            {interaction}
          </Content>
          <Control>{control}</Control>
        </Card>
      </Frame>
      <Debug>{debug}</Debug>
    </>
  );
};
