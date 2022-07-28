import { Address, ordAddress } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { CirclesGraph } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { toNullable } from '@circles-pink/state-machine/output/Data.Nullable';
import { neighborNodes } from '@circles-pink/state-machine/output/Data.IxGraph';
import React from 'react';
import {  hush } from '@circles-pink/state-machine/output/Data.Either';
import { pipe } from 'fp-ts/lib/function';

type Props = {
    graph : CirclesGraph,
    address: Address
}


export const TrustUserList = ({address, graph}: Props) => {

    const neighbors = neighborNodes(ordAddress)(address)(graph)

    const trustNodes = pipe(neighbors, hush, toNullable)

    if (!trustNodes) return <div>Address not found in graph</div>
   
    // const x = trustNodes

    return <div>Hello! tul</div>
};
