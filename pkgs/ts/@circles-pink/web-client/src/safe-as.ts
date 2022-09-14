import CirclesBN from '@circles-pink/state-machine/output/Data.BN';
import BN from 'bn.js';

export const toNativeBN = (x: CirclesBN.BN): BN => x as unknown as BN;
export const fromNativeBN = (x: BN): CirclesBN.BN =>
  x as unknown as CirclesBN.BN;
