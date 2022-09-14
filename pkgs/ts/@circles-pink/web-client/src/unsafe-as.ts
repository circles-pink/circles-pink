import {Json} from '@circles-pink/state-machine/src';

export const unsafeUnkownToJson = (x : unknown): Json => x as Json
