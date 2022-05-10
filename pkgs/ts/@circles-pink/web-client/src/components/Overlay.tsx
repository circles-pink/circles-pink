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
    <Frame {...props}>
      {content}
      <IconContainer onClick={() => closeOverlay()}>
        <Icon path={mdiCloseCircleOutline} size={2} color={theme.baseColor} />
      </IconContainer>
    </Frame>
  );
};

// -----------------------------------------------------------------------------
// UI / Frame
// -----------------------------------------------------------------------------

type FrameProps = OverlayProps;

const Frame = styled.div<FrameProps>(({ theme }: FrameProps) => [
  tw`absolute block p-8 shadow-xl rounded-xl`,
  css`
    background-color: ${'white'};
    border: 2px solid ${theme.baseColor};
    z-index: 100;
    top: 20%;
    width: 80%;
    margin-left: 10%;
    min-height: 6rem;
  `,
]);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const IconContainer = tw.div`absolute right-6 top-6 cursor-pointer`;
