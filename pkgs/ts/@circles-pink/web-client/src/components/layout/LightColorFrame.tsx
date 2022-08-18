import Icon from '@mdi/react';
import React, { ReactElement } from 'react';
import tw, { css, styled } from 'twin.macro';
import { Theme } from '../../context/theme';
import { JustifyBetween } from '../helper';
import { Claim } from '../text';

// -----------------------------------------------------------------------------
// UI / LightColorFrame
// -----------------------------------------------------------------------------

type FameProps = {
  theme: Theme;
  title?: string;
  icon?: string;
  children: ReactElement | ReactElement[] | string;
};

export const LightColorFrame = ({
  theme,
  title,
  icon,
  children,
}: FameProps): ReactElement => {
  return (
    <LightColorFrame_ theme={theme}>
      <Title>
        <JustifyBetween>
          <Claim color={theme.darkColor}>{title}</Claim>
          {icon ? (
            <Icon path={icon} size={1.5} color={theme.darkColor} />
          ) : (
            <></>
          )}
        </JustifyBetween>
      </Title>
      <>{children}</>
    </LightColorFrame_>
  );
};

const LightColorFrame_ = styled.div<FameProps>(({ theme }: FameProps) => [
  tw`block lg:p-8 md:p-8 p-4 border border-gray-800 shadow-xl rounded-xl`,
  css`
    background-color: ${theme.cardColor};
    max-width: calc(100vw - 3rem);
  `,
]);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const Title = tw.div`mb-4`;
