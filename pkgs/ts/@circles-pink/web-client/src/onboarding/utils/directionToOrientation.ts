import { Direction, _Direction } from '@circles-pink/state-machine/src';
import { Orientation } from 'anima-react';
import { pipe } from 'fp-ts/lib/function';

export const directionToOrientation = (direction: Direction): Orientation =>
  pipe(
    direction,
    _Direction.unDirection({
      onForwards: () => 'left',
      onBackwards: () => 'right',
    })
  );
