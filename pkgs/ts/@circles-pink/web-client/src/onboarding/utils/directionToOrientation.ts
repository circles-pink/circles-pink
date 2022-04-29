import { Orientation } from 'anima-react';
import { Direction } from 'generated/output/CirclesPink.Garden.StateMachine.Direction';

export const directionToOrientation = (direction: Direction): Orientation => {
  switch (direction.type) {
    case 'forwards':
      return 'left';
    case 'backwards':
      return 'right';
  }
};
