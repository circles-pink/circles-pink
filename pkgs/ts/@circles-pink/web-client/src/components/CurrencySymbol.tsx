import { keyframes } from '@emotion/react';
import React, { useEffect, useState } from 'react';
import { css, styled } from 'twin.macro';

// -----------------------------------------------------------------------------
// CurrencySymbol
// -----------------------------------------------------------------------------

type _CurrencySymbolProps = {
  color?: string;
  isLoading?: boolean;
  isRequesting?: boolean;
  size?: number;
};

type CurrencySymbolProps = Required<_CurrencySymbolProps>;

export const CurrencySymbol = (_props: _CurrencySymbolProps) => {
  const [isAnimPulse, setIsAnimPulse] = useState(false);
  const [isAnimRotate, setIsAnimRotate] = useState(false);

  const props = {
    color: _props.color || 'black',
    isLoading: isAnimPulse,
    isRequesting: isAnimRotate,
    size: _props.size || 2.5,
  };

  useEffect(() => {
    if (_props.isLoading) {
      setIsAnimPulse(true);
      setTimeout(() => {
        setIsAnimPulse(false);
      }, 2000);
    }
  }, [_props.isLoading, isAnimPulse]);

  useEffect(() => {
    if (_props.isRequesting) {
      setIsAnimRotate(true);
      setTimeout(() => {
        setIsAnimRotate(false);
      }, 2000);
    }
  }, [_props.isRequesting, isAnimRotate]);

  return (
    <ArcWrapper {...props}>
      <Arc
        className="start"
        isLoading={isAnimPulse}
        isRequesting={isAnimRotate}
        color={props.color}
        size={props.size}
      />
      <Arc
        className="end"
        isLoading={isAnimPulse}
        isRequesting={isAnimRotate}
        color={props.color}
        size={props.size}
      />
      <Dot {...props} />
    </ArcWrapper>
  );
};

// -----------------------------------------------------------------------------
// Arc
// -----------------------------------------------------------------------------

const ArcWrapper = styled.div<CurrencySymbolProps>(({ isRequesting, size }) => {
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
    width: ${size}rem;
    height: ${size}rem;

    animation: ${rotate} 2s infinite ease-in-out;
    animation-delay: 0s;
    animation-direction: forwards;
  `;
});

const Arc = styled.div<CurrencySymbolProps>(({ color, size }) => {
  return css`
    position: absolute;
    width: ${size}rem;
    height: ${size}rem;
    border-radius: 100%;
    border: ${size / 7.5}rem solid;
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

const Dot = styled.div<CurrencySymbolProps>(({ color, isLoading, size }) => {
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
    width: ${size / 3}rem;
    height: ${size / 3}rem;
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
