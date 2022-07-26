import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { CirclesGraph } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { neighborNodes } from '@circles-pink/state-machine/output/Data.IxGraph';
import React from 'react';

type Props = {
    graph : CirclesGraph,
    address: Address
}


export const TrustUserList = (props: Props) => {

    const neighbors = neighborNodes()
   
    return <div>Hello! tul</div>
};
