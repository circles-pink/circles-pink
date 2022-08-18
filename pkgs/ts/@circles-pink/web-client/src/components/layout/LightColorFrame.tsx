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
      <HeadSection>
        <JustifyBetween>
          <CardTitle color={theme.darkColor}>{title}</CardTitle>
          {icon ? (
            <Icon path={icon} size={1.5} color={theme.darkColor} />
          ) : (
            <></>
          )}
        </JustifyBetween>
      </HeadSection>
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

const HeadSection = tw.div`mb-4`;

type CardTitleProps = {
  color: string;
};
export const CardTitle = styled.h2<CardTitleProps>(({ color }) => [
  tw`mb-2 text-3xl sm:text-4xl font-extrabold tracking-tight block`,
  css`
    color: ${color};
    margin: 0;
    padding: 0;
  `,
]);
