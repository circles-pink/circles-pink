import * as E from 'fp-ts/Either';
import * as O from 'fp-ts/Option';
import { Either } from '@circles-pink/state-machine/output/Data.FpTs.Either';
import { Option } from '@circles-pink/state-machine/output/Data.FpTs.Option';
import { Tuple } from '@circles-pink/state-machine/output/Data.FpTs.Tuple';

export const fromFpTsEither = <A, B>(e: E.Either<A, B>): Either<A, B> =>
  e as unknown as Either<A, B>;

export const toFpTsEither = <A, B>(e: Either<A, B>): E.Either<A, B> =>
  e as unknown as E.Either<A, B>;

export const fromFpTsOption = <A>(e: O.Option<A>): Option<A> =>
  e as unknown as Option<A>;

export const toFpTsOption = <A>(e: Option<A>): O.Option<A> =>
  e as unknown as O.Option<A>;

export const fromFpTsTuple = <A, B>(e: [A, B]): Tuple<A, B> =>
  e as unknown as Tuple<A, B>;

export const toFpTsTuple = <A, B>(e: Tuple<A, B>): [A, B] =>
  e as unknown as [A, B];
