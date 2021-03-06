import React, { ReactElement, useContext } from 'react';
import tw, { css, styled } from 'twin.macro';
import { DebugContext } from '../context/debug';
import { Theme, ThemeContext } from '../context/theme';

// -----------------------------------------------------------------------------
// DialogCard
// -----------------------------------------------------------------------------

type DialogCardProps = {
  header?: ReactElement;
  text?: ReactElement;
  interaction?: ReactElement;
  control?: ReactElement;
  mainContent?: ReactElement;
  debug?: ReactElement;
};

export const DialogCard = ({
  header,
  text,
  interaction,
  control,
  mainContent,
  debug,
}: DialogCardProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const [debugContext] = useContext(DebugContext);

  return (
    <>
      <Frame theme={theme}>
        <Header>{header}</Header>
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
      {debugContext && <Debug>{debug}</Debug>}
    </>
  );
};

// -----------------------------------------------------------------------------
// UI / Frame
// -----------------------------------------------------------------------------

type FrameProps = {
  theme: Theme;
};

const Frame = styled.div((props: FrameProps) => [
  // tw`border border-dashed`,
  // css`border-color: ${props.theme.baseColor};`,
  css`
    background-color: ${props.theme.bgColor};
  `,
]);

// -----------------------------------------------------------------------------
// UI / Layout Components
// -----------------------------------------------------------------------------

const Header = tw.div``;
const CardHead = tw.div`max-w-7xl mx-auto pt-8 pb-8 px-4 sm:px-6 lg:pt-16 lg:pb-4 lg:px-8 lg:flex lg:justify-between`;
const CardBody = tw.div`max-w-7xl mx-auto pb-8 px-4 sm:px-6 lg:pb-16 lg:px-8`;
const IntroContent = tw.div`lg:w-1/2`;
const MainContent = tw.div`w-full`;
const Control = tw.div`flex mt-8 lg:flex-shrink-0 lg:justify-end`;
const Debug = tw.div`p-8`;
