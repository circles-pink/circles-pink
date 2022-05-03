import tw, { css, styled } from 'twin.macro';
import React, { ReactElement } from 'react';
import { Claim } from './text';
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
      {/* <Claim color={themeColor}>{title}</Claim> */}
      {content}
      <IconContainer onClick={() => closeOverlay()}>
        <Icon path={mdiCloseCircleOutline} size={2} color={theme.baseColor} />
      </IconContainer>
    </Frame>
  );
};

type FrameProps = OverlayProps;

const Frame = styled.div<FrameProps>(({ theme }: FrameProps) => [
  tw`absolute block p-8 shadow-xl rounded-xl`,
  css`
    background-color: ${'white'};
    border: 2px solid ${theme.baseColor};
    z-index: 50;
    top: 5%;
    width: 80%;
    margin-left: 10%;
    min-height: 6rem;
  `,
]);
const IconContainer = tw.div`absolute bottom-8 right-6 top-6 cursor-pointer`;
