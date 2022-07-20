import React, { ReactElement, useContext } from 'react';
import tw, { css, styled } from 'twin.macro';
import { DebugContext } from '../context/debug';
import { Theme, ThemeContext } from '../context/theme';

// -----------------------------------------------------------------------------
// UserDashboard
// -----------------------------------------------------------------------------

type UserDashboardProps = {
  header?: ReactElement;
  text?: ReactElement;
  interaction?: ReactElement;
  control?: ReactElement;
  mainContent?: ReactElement;
  overlay?: ReactElement | null;
  debug?: ReactElement | ReactElement[];
};

export const UserDashboard = ({
  header,
  text,
  control,
  mainContent,
  overlay,
  debug,
}: UserDashboardProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const [debugContext] = useContext(DebugContext);

  return (
    <>
      <Frame theme={theme}>
        {overlay}
        <Header>{header}</Header>
        <CardHead>
          <IntroContent>{text}</IntroContent>
          <Control>{control}</Control>
        </CardHead>

        <CardBody>{mainContent}</CardBody>
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
const CardHead = tw.div`max-w-7xl mx-auto pb-8 px-4 sm:px-6 lg:pb-16 lg:px-8 grid lg:grid-cols-2 md:grid-cols-2 lg:gap-4 md:gap-4`;
const CardBody = tw.div`max-w-7xl mx-auto pb-8 px-4 sm:px-6 lg:pb-16 lg:px-8`;
const IntroContent = tw.div`flex md:mt-2 lg:mt-2`;
const Control = tw.div`flex lg:mt-2 lg:justify-end md:justify-end`;
const Debug = tw.div`p-8`;
