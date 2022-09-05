import React, { ReactChildren, ReactElement, ReactNode } from 'react';
import tw, { css, styled } from 'twin.macro';
import { Claim } from './text';
import Icon from '@mdi/react';
import { darken } from '../onboarding/utils/colorUtils';
import { Theme } from '../context/theme';

export type FrameProps = {
  theme: Theme;
  title?: string;
  icon?: string;
  children?: ReactNode;
};

export const Frame = ({
  theme,
  title,
  icon,
  children,
}: FrameProps): ReactElement => {
  return (
    <FrameRoot theme={theme}>
      <Title>
        <JustifyBetween>
          <Claim color={darken(theme.lightColor, 2)}>{title}</Claim>
          {icon && (
            <Icon path={icon} size={1.5} color={darken(theme.lightColor, 2)} />
          )}
        </JustifyBetween>
      </Title>
      {children}
    </FrameRoot>
  );
};

const FrameRoot = styled.div<FrameProps>(({ theme }: FrameProps) => [
  tw`block lg:p-8 md:p-8 p-4 border border-gray-800 shadow-xl rounded-xl`,
  css`
    background-color: ${theme.lightColor};
  `,
]);

const Title = tw.div`mb-4`;

const JustifyBetween = tw.div`flex justify-between`;
