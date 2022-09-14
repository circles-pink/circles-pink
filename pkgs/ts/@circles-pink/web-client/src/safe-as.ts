import CirclesBN from '@circles-pink/state-machine/output/Data.BN';
import { CirclesConfigEffect, Unit, Either } from '@circles-pink/state-machine/src';
import BN from 'bn.js';

export const toNativeBN = (x: CirclesBN.BN): BN => x as unknown as BN;
export const fromNativeBN = (x: BN): CirclesBN.BN =>
  x as unknown as CirclesBN.BN;

type NativeCirclesConfigEffect = {
    extractEmail : Either<String, (_: String) => Effect<Unit>)>
    // onTrackingEvent :: Maybe (TrackingEvent -> m Unit)
    // onTrackingResumee :: Maybe ((Resumee -> Resumee) -> m Unit)
    // safeAddress :: Maybe Address
    // strictMode :: Boolean
};

export const toNativeCirclesConfigEffect = (
  x: CirclesConfigEffect
): NativeCirclesConfigEffect => x as unknown as NativeCirclesConfigEffect;

export const fromNativeCirclesConfigEffect = (
  x: NativeCirclesConfigEffect
): CirclesConfigEffect => x as unknown as CirclesConfigEffect;
