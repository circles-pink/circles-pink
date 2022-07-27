import { Address, ordAddress } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { CirclesGraph } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { ordTrustNode } from '@circles-pink/state-machine/output/CirclesPink.Data.TrustNode';

import { neighborNodes } from '@circles-pink/state-machine/output/Data.IxGraph';
import React from 'react';

type Props = {
    graph : CirclesGraph,
    address: Address
}


export const TrustUserList = ({address, graph}: Props) => {

    const neighbors = neighborNodes(ordAddress)(address)(graph)
   
    return <div>Hello! tul</div>
};
