import styled from '@emotion/styled';
import { count } from 'console';
import React, { ReactNode } from 'react';
import { css } from 'twin.macro';
import { range } from 'fp-ts/lib/NonEmptyArray';

// -----------------------------------------------------------------------------
// UI / Item
// -----------------------------------------------------------------------------

type ItemProps = Props & { index: number };

const wrapOffset = (fac: number): number => (fac >= 1 ? fac - 1 : fac);

const Item = styled.div<ItemProps>(({ width, count, color, index }) => {
  const height = width / count;
  const time = 15;
  const offset = index * 0.1; //index * (1 / count);
  const name = `pulseDot${index}`;
  console.log(wrapOffset(offset + 0), wrapOffset(offset + 0.5));
  return css`
    width: ${width}px;
    height: ${height}px;
    background-color: ${color};
    border-radius: 50%;
    animation: ${name} ${time}s infinite linear;

    @keyframes ${name} {
      0% {
        transform: scale(${wrapOffset(offset + 0)});
      }
      25% {
        transform: scale(${wrapOffset(offset + 0.5)});
      }
      50% {
        transform: scale(${wrapOffset(offset + 1)});
      }
      75% {
        transform: scale(${wrapOffset(offset + 0.5)});
      }
      100% {
        transform: scale(${wrapOffset(offset + 0)});
      }
    }
  `;
});

// -----------------------------------------------------------------------------
// UI / Root
// -----------------------------------------------------------------------------

type RootProps = Props;

const Root = styled.div<RootProps>(({ width, count }) => {
  const height = width / count;
  return css`
    width: ${width}px;
    height: ${height}px;
    display: flex;
  `;
});

// -----------------------------------------------------------------------------
// UI / LoadingCircles
// -----------------------------------------------------------------------------

type LoadingCirclesProps = {
  width?: number;
  count?: number;
  debug?: boolean;
  color?: string;
};

type Props = Required<LoadingCirclesProps>;

const normProps = ({
  width,
  count,
  debug,
  color,
}: LoadingCirclesProps): Props => ({
  width: withDefault(width, 100),
  count: withDefault(count, 3),
  debug: withDefault(debug, false),
  color: withDefault(color, 'red'),
});

export const LoadingCircles = (props_: LoadingCirclesProps) => {
  const props = normProps(props_);
  const { count } = props;

  return (
    <Root {...props}>
      {range(0, count - 1).map(index => (
        <Item {...props} index={index}></Item>
      ))}
    </Root>
  );
};

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

const withDefault = <T,>(x: T | undefined, def: T): T =>
  x === undefined ? def : x;
