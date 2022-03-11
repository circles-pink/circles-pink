import React, { ReactElement, useContext } from 'react';
import tw, { styled, theme } from 'twin.macro';
import { ThemeContext } from '../context/theme';

type FrameProps = {
  borderColor: string;
};

const Frame = styled.div((props: FrameProps) => [
  `bg-gray-50 border border-dotted`,
  `border-color: ${props.borderColor}`,
]);

const CardHead = tw.div`max-w-7xl mx-auto pt-12 pb-8 px-4 sm:px-6 lg:pt-16 lg:pb-4 lg:px-8 lg:flex lg:justify-between`;
const CardBody = tw.div`max-w-7xl mx-auto pb-12 px-4 sm:px-6 lg:pb-16 lg:px-8`;
const IntroContent = tw.div`lg:w-1/2`;
const MainContent = tw.div`w-full`;
const Control = tw.div`mt-8 flex lg:mt-0 lg:flex-shrink-0 lg:mt-8`;
const Debug = tw.div`p-8`;

type DemoCardProps = {
  text: ReactElement;
  interaction?: ReactElement;
  control: ReactElement;
  mainContent?: ReactElement;
  debug?: ReactElement;
};

export const DialogCard = ({
  text,
  interaction,
  control,
  mainContent,
  debug,
}: DemoCardProps): ReactElement => {
  const { theme } = useContext(ThemeContext);

  return (
    <>
      <Frame borderColor={theme.baseColor}>
        <CardHead>
          <IntroContent>
            {text}
            {interaction}
          </IntroContent>
          <Control>{control}</Control>
        </CardHead>

        <CardBody>
          <MainContent>{mainContent}</MainContent>
        </CardBody>
      </Frame>
      <Debug>{debug}</Debug>
    </>
  );
};
