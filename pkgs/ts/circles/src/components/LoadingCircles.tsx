import styled from '@emotion/styled';
import { count } from 'console';
import React, { ReactElement, ReactNode } from 'react';
import { css } from 'twin.macro';
import { range } from 'fp-ts/lib/NonEmptyArray';
import { keyframes } from '@emotion/react';
import { normalizeUI, withDefaults } from '../ui-utils';

// -----------------------------------------------------------------------------
// UI / Item
// -----------------------------------------------------------------------------

type ItemProps = NormProps & { index: number };

const Item = styled.div<ItemProps>(
  ({ width, count, color, index, speed, maxScale, done, colorDone, pulse }) => {
    const height = width / count;
    const offset = index * (1 / count);
    const delay = offset * speed * 0.5;

    const anim = keyframes`
      0%,${pulse * 100}% {
        transform: scale(0);
      }
      100% {
        transform: scale(${maxScale});
      }
  `;

    return css`
      width: ${width}px;
      height: ${height}px;
      transition: background-color 1s;
      background-color: ${done ? colorDone : color};
      border-radius: 50%;
      transform: scale(0);
      animation: ${anim} ${speed}s infinite ease-in-out;
      animation-delay: ${delay}s;
      animation-direction: alternate;
    `;
  }
);

// -----------------------------------------------------------------------------
// UI / Root
// -----------------------------------------------------------------------------

type RootProps = NormProps;

const Root = styled.div<RootProps>(({ width, count }) => {
  const height = width / count;
  return css`
    width: ${width}px;
    height: ${height}px;
    display: flex;
  `;
});

// -----------------------------------------------------------------------------
// UI / Norm
// -----------------------------------------------------------------------------

type NormProps = Required<LoadingCirclesProps>;

const Norm = (props: NormProps) => {
  const { count } = props;
  return (
    <Root {...props}>
      {range(0, count - 1).map(index => (
        <Item {...props} index={index} />
      ))}
    </Root>
  );
};

// -----------------------------------------------------------------------------
// UI / LoadingCircles
// -----------------------------------------------------------------------------

type LoadingCirclesProps = {
  width?: number;
  count?: number;
  color?: string;
  colorDone?: string;
  speed?: number;
  maxScale?: number;
  done?: boolean;
  pulse?: number;
};

const normProps = (props: LoadingCirclesProps): NormProps =>
  withDefaults(props, {
    width: 100,
    count: 3,
    color: 'pink',
    colorDone: 'lime',
    speed: 0.9,
    maxScale: 0.5,
    done: false,
    pulse: 0.2,
  });

export const LoadingCircles = normalizeUI(normProps, Norm);
