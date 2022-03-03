import { ClassNames, keyframes } from '@emotion/react';
import React, { ReactElement } from 'react';

export type Direction = 'left' | 'right' | 'up' | 'down';

type FadeInProps = {
  direction?: Direction;
  delay?: number;
  children: ReactElement;
};

export const FadeIn = ({
  direction = 'left',
  delay = 0,
  children,
}: FadeInProps): ReactElement => {
  return (
    <ClassNames>
      {({ css, cx }) => (
        <>
          {React.cloneElement(children, {
            className: css`
              opacity: 0;
              animation: ${fadeIn(direction)} 0.75s ease ${delay}s forwards;
            `,
          })}
        </>
      )}
    </ClassNames>
  );
};

const fadeIn = (direction: Direction): string => {
  return keyframes`
    from {
        opacity:0;
        transform: ${getCssProp(direction)};
    }
    to {
        opacity:1;
        transform: translatex(0);
    }
`;
};

const getCssProp = (direction: Direction) => {
  switch (direction) {
    case 'left':
      return 'translatex(-10px)';
    case 'right':
      return 'translatex(10px)';
    case 'up':
      return 'translatey(-10px)';
    case 'down':
      return 'translatey(10px)';
  }
};
