import * as E from 'fp-ts/Either';
import * as O from 'fp-ts/Option';
import { Either } from '@circles-pink/state-machine/output/Data.FpTs.Either';
import { Unit } from '@circles-pink/state-machine/output/Data.Unit';
import { Option } from '@circles-pink/state-machine/output/Data.FpTs.Option';

export const fromFpTsEither = <A, B>(e: E.Either<A, B>): Either<A, B> =>
  e as unknown as Either<A, B>;

export const toFpTsEither = <A, B>(e: Either<A, B>): E.Either<A, B> =>
  e as unknown as E.Either<A, B>;

export const fromFpTsOption = <A>(e: O.Option<A>): Option<A> =>
  e as unknown as Option<A>;

export const toFpTsOption = <A>(e: Option<A>): O.Option<A> =>
  e as unknown as O.Option<A>;
