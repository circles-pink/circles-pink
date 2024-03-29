import React from 'react';
import { css, styled } from 'twin.macro';
import { Theme } from '../../context/theme';

type HeadingPrps = {
  children: string;
  theme?: Theme;
};

export const Heading = ({ children }: HeadingPrps) => {
  return <Headline>{children}</Headline>;
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const Headline = styled.span(() => {
  return [
    css`
      font-weight: 600;
    `,
  ];
});
