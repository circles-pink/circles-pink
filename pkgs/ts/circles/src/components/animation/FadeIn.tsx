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
        <div
          className={css`
            opacity: 0;
            animation: ${fadeIn(direction)} 0.2s ease-in-out ${delay}s forwards;
          `}
        >
          {children}
        </div>
      )}
    </ClassNames>
  );
};

const fadeIn = (direction: Direction): string => {
  return keyframes`
    from {
        opacity:0;
        transform: ${getTransform(direction)};
    }
    to {
        opacity:1;
        transform: translatex(0);
    }
`;
};

const getTransform = (direction: Direction) => {
  switch (direction) {
    case 'left':
      return 'translatex(-10px)';
    case 'right':
      return 'translatex(10px)';
    case 'up':
      return 'translatey(10px)';
    case 'down':
      return 'translatey(-10px)';
  }
};
