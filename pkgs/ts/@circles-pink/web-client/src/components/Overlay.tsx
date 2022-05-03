import tw, { css, styled } from 'twin.macro';
import React, { ReactElement } from 'react';
import { Claim } from './text';
import Icon from '@mdi/react';
import { Theme } from '../context/theme';

type OverlayProps = {
  content?: ReactElement | ReactElement[] | string;
  theme?: Theme;
};

export const Overlay = (props: OverlayProps) => {
  const { content, theme } = props;
  return (
    <Frame {...props}>
      {/* <Claim color={themeColor}>{title}</Claim> */}
      {content}
      {/* <IconContainer>
        <Icon path={icon} size={2} color={themeColor} />
      </IconContainer> */}
    </Frame>
  );
};

type FrameProps = OverlayProps;

const Frame = styled.div<FrameProps>(({ theme }) => {
  return [
    tw`absolute block p-8 pb-24 rounded-sm shadow-xl`,
    css`
      border-top: solid 4px ${theme.baseColor};
      flex-grow: 1;
      flex-basis: 0;
    `,
  ];
});
const IconContainer = tw.div`absolute bottom-8 right-8`;
