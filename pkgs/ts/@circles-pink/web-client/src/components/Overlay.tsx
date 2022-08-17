import tw, { css, styled } from 'twin.macro';
import React, { ReactElement } from 'react';
import Icon from '@mdi/react';
import { Theme } from '../context/theme';
import { mdiCloseCircleOutline } from '@mdi/js';

type OverlayProps = {
  content?: ReactElement | ReactElement[] | string;
  closeOverlay: () => void;
  theme: Theme;
};

export const Overlay = (props: OverlayProps) => {
  const { content, theme, closeOverlay } = props;
  return (
    <Envelope>
      <Frame {...props}>
        {content}
        <IconContainer onClick={() => closeOverlay()}>
          <Icon path={mdiCloseCircleOutline} size={2} color={theme.baseColor} />
        </IconContainer>
      </Frame>
    </Envelope>
  );
};

// -----------------------------------------------------------------------------
// UI / Frame
// -----------------------------------------------------------------------------

type FrameProps = OverlayProps;

const Frame = styled.div<FrameProps>(({ theme }: FrameProps) => [
  tw`block p-8 shadow-xl rounded-xl lg:w-3/5 w-11/12 mx-auto`,
  css`
    background-color: ${'white'};
    border: 2px solid ${theme.baseColor};
    z-index: 100;
    position: fixed;
    top: 50%;
    left: 50%;
    /* bring your own prefixes */
    transform: translate(-50%, -50%);
    min-height: 6rem;
  `,
]);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const Envelope = tw.div`flex justify-center items-center`;
const IconContainer = tw.div`absolute right-6 top-6 cursor-pointer`;
