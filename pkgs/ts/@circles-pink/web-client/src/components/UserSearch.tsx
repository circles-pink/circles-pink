import { CirclesGraph, UserSearchResult } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import React from 'react';

type Props = {
    trusts : CirclesGraph
    userSearchResult: UserSearchResult
}


export const UserSearch = (props: Props) => {
   
    return <div>
        <input type={"text"}/>
        <button>Search</button>
    </div>
};
