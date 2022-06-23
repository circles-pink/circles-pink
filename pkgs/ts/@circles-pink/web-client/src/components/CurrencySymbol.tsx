import { keyframes } from '@emotion/react';
import React, { useEffect, useState } from 'react';
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
  color?: string;
  isLoading?: boolean;
  isRequesting?: boolean;
};

export const CurrencySymbol = ({
  color = 'black',
  isLoading = false,
  isRequesting = false,
}: CurrencySymbolProps) => {
  const [isAnimPulse, setIsAnimPulse] = useState(false);
  const [isAnimRotate, setIsAnimRotate] = useState(false);

  useEffect(() => {
    if (isLoading) {
      setIsAnimPulse(true);
      setTimeout(() => {
        setIsAnimPulse(false);
      }, 2000);
    }
  }, [isLoading, isAnimPulse]);

  useEffect(() => {
    if (isRequesting) {
      setIsAnimRotate(true);
      setTimeout(() => {
        setIsAnimRotate(false);
      }, 2000);
    }
  }, [isRequesting, isAnimRotate]);

  return (
    <ArcWrapper isRequesting={isAnimRotate}>
      <Arc className="start" color={color} />
      <Arc className="end" color={color} />
      <Dot color={color} isLoading={isAnimPulse} />
    </ArcWrapper>
  );
};

// -----------------------------------------------------------------------------
// Arc
// -----------------------------------------------------------------------------

type ArcWrapperProps = {
  isRequesting?: boolean;
};

const ArcWrapper = styled.div<ArcWrapperProps>(({ isRequesting }) => {
  const rotate = isRequesting
    ? keyframes(
        css`
          0%,
          50% {
            transform: rotate(0deg);
          }
          100% {
            transform: rotate(360deg);
          }
        `
      )
    : keyframes(css``);

  return css`
    position: relative;
    margin: 0;
    width: ${width}rem;
    height: ${height}rem;

    animation: ${rotate} 2s infinite ease-in-out;
    animation-delay: 0s;
    animation-direction: forwards;
  `;
});

type ArcProps = {
  color: string;
};

const Arc = styled.div<ArcProps>(({ color }) => {
  return css`
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
  `;
});

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

// const Dot = styled.div<DotProps>(({ color, isLoading }) => {
//   const dotWidth = width / 3;
//   const dotHeight = height / 3;

//   const pulse = isLoading
//     ? keyframes(
//         css`
//           0%,
//           100% {
//             transform: scale(1);
//           }
//           100% {
//             transform: scale(0);
//           }
//         `
//       )
//     : keyframes(css``);

//   return css`
//     width: ${dotWidth}rem;
//     height: ${dotHeight}rem;
//     background-color: ${color};
//     border-radius: 50%;
//     position: absolute;
//     top: calc(50% - ${dotWidth / 4}rem);
//     left: calc(50% - ${dotHeight / 4}rem);
//     transform: scale(1);
//     animation: ${pulse} 1s infinite ease-in-out;
//     animation-delay: 0s;
//     animation-direction: alternate;
//   `;
// });
