import React, { ReactElement, useContext } from 'react';
import tw, { css, styled } from 'twin.macro';
import { DebugContext } from '../context/debug';
import { Theme, ThemeContext } from '../context/theme';

// -----------------------------------------------------------------------------
// XbgeDialogCard
// -----------------------------------------------------------------------------

type XbgeDialogCardProps = {
  mainContent?: ReactElement;
  debug?: ReactElement;
};

export const XbgeDialogCard = ({
  mainContent,
  debug,
}: XbgeDialogCardProps): ReactElement => {
  const [theme] = useContext(ThemeContext);
  const [debugContext] = useContext(DebugContext);

  return (
    <>
      <Frame theme={theme}>
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

const CardBody = tw.div`max-w-7xl mx-auto p-4 sm:p-6 lg:p-8`;
const MainContent = tw.div`w-full`;
const Debug = tw.div`p-8`;
