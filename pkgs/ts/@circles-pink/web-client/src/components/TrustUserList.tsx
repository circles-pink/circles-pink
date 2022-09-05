import {
  Address,
  ordAddress,
} from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import * as G from '@circles-pink/state-machine/output/Data.IxGraph';
import React from 'react';
import { _Tuple, _Either, _Nullable } from '@circles-pink/state-machine/src';
import * as TN from '@circles-pink/state-machine/output/CirclesPink.Data.TrustNode';
import * as UI from '@circles-pink/state-machine/output/CirclesPink.Data.UserIdent';
import * as A from '@circles-pink/state-machine/output/Simple.Data.Array';
import { CirclesGraph } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { pipe } from 'fp-ts/lib/function';

type Props = {
  graph: CirclesGraph;
  address: Address;
};

export const TrustUserList = ({ address, graph }: Props) => {
  const neighborhood = G.neighborhood(ordAddress)(address)(graph);
  const items = pipe(neighborhood, _Either.hush, _Nullable.toNullable);

  if (!items) return <div>Address not found in graph!</div>;

  return (
    <div>
      {pipe(
        items,
        A.mapArray(x => {
          const { userIdent } = _Tuple.unTuple(
            () => (trustNode: TN.TrustNode) => TN.unwrap(trustNode)
          )(x);

          const id = UI.getIdentifier(userIdent);

          return <div>{id}</div>;
        })
      )}
    </div>
  );
};
