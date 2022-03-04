import { Direction } from "generated/output/CirclesPink.Garden.StateMachine.Direction";
import { Orientation } from "../components/animation/FadeIn";

export const directionToOrientation = (direction: Direction): Orientation => {
    switch (direction.type) {
      case 'forwards':
        return 'left';
      case 'backwards':
        return 'right';
    }
  };