import tw, { css, styled } from 'twin.macro';
import React from 'react';

type ThreeDotsProps = {
  color?: string;
  width?: number;
};
const withDefault = <T,>(x: T | undefined, def: T): T =>
  x === undefined ? def : x;

const normProps = ({
  color,
  width,
}: ThreeDotsProps): Required<ThreeDotsProps> => ({
  color: withDefault(color, 'red'),
  width: withDefault(width, 60),
});

type RootProps = {
  width: number;
};

const Root = styled.div<RootProps>(
  ({ width }) => `
  outline: 1px solid red;
  width: ${width}px
`
);

const ThreeDotsPulse_ = styled.div<Required<ThreeDotsProps>>(
  ({ color, width }) => {
    const widthPoint = Math.round(width / 3);

    return [
      css`
        position: relative;
        left: -9999px;
        width: 20px;
        height: 20px;
        margin-left: ${widthPoint}px;
        margin-right: ${widthPoint}px;
        border-radius: 10px;
        background-color: ${color};
        color: #9880ff;
        box-shadow: 9999px 0 0 -5px #9880ff;
        animation: dotPulse 1.5s infinite linear;
        animation-delay: 0.25s;
        &:before,
        &:after {
          content: '';
          display: inline-block;
          position: absolute;
          top: 0;
          width: 10px;
          height: 10px;
          border-radius: 5px;
          background-color: #9880ff;
          color: #9880ff;
        }
        &:before {
          box-shadow: 9984px 0 0 -5px #9880ff;
          animation: dotPulseBefore 1.5s infinite linear;
          animation-delay: 0s;
        }
        &:after {
          box-shadow: 10014px 0 0 -5px #9880ff;
          animation: dotPulseAfter 1.5s infinite linear;
          animation-delay: 0.5s;
        }

        @keyframes dotPulseBefore {
          0% {
            box-shadow: 9984px 0 0 -5px #9880ff;
          }
          30% {
            box-shadow: 9984px 0 0 2px #9880ff;
          }
          60%,
          100% {
            box-shadow: 9984px 0 0 -5px #9880ff;
          }
        }

        @keyframes dotPulse {
          0% {
            box-shadow: 9999px 0 0 -5px #9880ff;
          }
          30% {
            box-shadow: 9999px 0 0 2px #9880ff;
          }
          60%,
          100% {
            box-shadow: 9999px 0 0 -5px #9880ff;
          }
        }

        @keyframes dotPulseAfter {
          0% {
            box-shadow: 10014px 0 0 -5px #9880ff;
          }
          30% {
            box-shadow: 10014px 0 0 2px #9880ff;
          }
          60%,
          100% {
            box-shadow: 10014px 0 0 -5px #9880ff;
          }
        }
      `,
      // tw`font-bold py-2 px-4 rounded-full mr-1 cursor-pointer`,
    ];
  }
);

export const ThreeDotsPulse = (props_: ThreeDotsProps) => {
  const props = normProps(props_);
  const { width } = props;
  return (
    <Root width={width}>
      <ThreeDotsPulse_ {...props} />
    </Root>
  );
};
