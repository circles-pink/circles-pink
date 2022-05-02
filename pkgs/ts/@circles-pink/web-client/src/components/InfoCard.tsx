import tw, { css, styled } from 'twin.macro';
import React, { ReactElement } from 'react';
import { Claim } from './text';
import Icon from '@mdi/react';

type InfoCardProps = {
  title: string;
  themeColor: string;
  text: ReactElement | ReactElement[] | string;
  icon: any;
};

export const InfoCard = (props: InfoCardProps) => {
  const { title, themeColor, text, icon } = props;
  return (
    <Frame {...props}>
      <Claim color={themeColor}>{title}</Claim>
      {text}
      <IconContainer>
        <Icon path={icon} size={2} color={themeColor} />
      </IconContainer>
    </Frame>
  );
};

type FrameProps = InfoCardProps;

const Frame = styled.div<FrameProps>(({ themeColor }) => {
  return [
    tw`relative block p-8 pb-24 rounded-sm shadow-xl`,
    css`
      border-top: solid 4px ${themeColor};
      flex-grow: 1;
      flex-basis: 0;
    `,
  ];
});
const IconContainer = tw.div`absolute bottom-8 right-8`;
