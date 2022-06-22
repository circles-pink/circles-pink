import { keyframes } from '@emotion/react';
import React from 'react';
import { css, styled } from 'twin.macro';

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

const width = 2.25;
const height = 2.25;

// -----------------------------------------------------------------------------
// CurrencySymbol
// -----------------------------------------------------------------------------

type CurrencySymbolProps = {
  color: string;
  isLoading: boolean;
};

export const CurrencySymbol = ({ color, isLoading }: CurrencySymbolProps) => {
  return (
    <ArcWrapper>
      <Arc className="start" color={color} />
      <Arc className="end" color={color} />
      <Dot color={color} isLoading={isLoading} />
    </ArcWrapper>
  );
};

// -----------------------------------------------------------------------------
// Arc
// -----------------------------------------------------------------------------

const ArcWrapper = styled.div(() => [
  css`
    position: relative;
    margin: 0;
    width: ${width}rem;
    height: ${height}rem;
  `,
]);

type ArcProps = {
  color: string;
};

const Arc = styled.div<ArcProps>(({ color }) => [
  css`
    position: absolute;
    width: ${width}rem;
    height: ${height}rem;
    border-radius: 100%;
    border: 1px solid;
    border: 4px solid;
    border-color: ${color};

    &.start {
      border-color: ${color} transparent;
      -webkit-transform: rotate(0deg);
      -moz-transform: rotate(0deg);
      -ms-transform: rotate(0deg);
      -o-transform: rotate(0deg);
      transform: rotate(0deg);
    }

    &.end {
      border-color: transparent ${color} ${color} ${color};
      -webkit-transform: rotate(90deg);
      -moz-transform: rotate(90deg);
      -ms-transform: rotate(90deg);
      -o-transform: rotate(90deg);
      transform: rotate(90deg);
    }
  `,
]);

// -----------------------------------------------------------------------------
// Dot
// -----------------------------------------------------------------------------

type DotProps = {
  color: string;
  isLoading: boolean;
};

const Dot = styled.div<DotProps>(({ color, isLoading }) => {
  const pulse = isLoading
    ? keyframes(
        css`
          0%,
          100% {
            transform: translate(-50%, -50%) scale(1);
          }
          100% {
            transform: translate(-50%, -50%) scale(0);
          }
        `
      )
    : keyframes(css``);

  return css`
    width: ${width / 3}rem;
    height: ${height / 3}rem;
    background-color: ${color};
    border-radius: 50%;
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%) scale(1);
    animation: ${pulse} 1s infinite ease-in-out;
    animation-delay: 0s;
    animation-direction: alternate;
  `;
});
