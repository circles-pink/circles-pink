import React, { ReactElement, useContext } from 'react';
import tw, { css, styled } from 'twin.macro';
import { DebugContext } from '../context/debug';
import { Theme, ThemeContext } from '../context/theme';

// -----------------------------------------------------------------------------
// XbgeUserDashboard
// -----------------------------------------------------------------------------

type XbgeUserDashboardProps = {
  mainContent?: ReactElement;
  overlay?: ReactElement | null;
  debug?: ReactElement | ReactElement[];
};

export const XbgeUserDashboard = ({
  mainContent,
  overlay,
  debug,
}: XbgeUserDashboardProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const [debugContext] = useContext(DebugContext);

  return (
    <>
      <Frame theme={theme}>
        {overlay}
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

const CardBody = tw.div`max-w-7xl my-4 mx-auto px-4 sm:px-6 lg:px-8`;
const Debug = tw.div`p-8`;
