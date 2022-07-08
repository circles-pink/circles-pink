import React from 'react';
import { styled } from 'twin.macro';
import { defaultTheme, Theme } from '../../context/theme';

type HeadingPrps = {
  children: string;
  theme?: Theme;
};

export const Heading = ({ children, theme = defaultTheme }: HeadingPrps) => {
  return <Headline>{children}</Headline>;
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const Headline = styled.h2(() => {
  return [];
});
