import {
  UserIdent,
  fromUser,
  getIdentifier,
  getAddress,
} from '@circles-pink/state-machine/output/CirclesPink.Data.UserIdent';
import { TrustConnection } from '@circles-pink/state-machine/output/CirclesPink.Data.TrustConnection';
import * as G from '@circles-pink/state-machine/output/Data.IxGraph';
import {
  CirclesGraph,
  DashboardState,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import * as RD from '@circles-pink/state-machine/output/RemoteData';
import * as A from '@circles-pink/state-machine/output/Simple.Data.Array';

import { pipe } from 'fp-ts/lib/function';
import React from 'react';
import { fields, matchADT, matchV } from '../purs-util';
import {
  Address,
  ordAddress,
} from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { Pair } from '@circles-pink/state-machine/output/Data.Pair';
import { match } from 'assert';

// -----------------------------------------------------------------------------
// UI / Row
// -----------------------------------------------------------------------------

type RowProps = { userIdent: UserIdent } & UserSearchProps;

const Row = ({ centerAddress, userIdent, trusts, onAddTrust }: RowProps) => {
  const targetAddress = getAddress(userIdent);

  const outgoingEdge = pipe(
    trusts,
    G.lookupEdge(ordAddress)(Pair.create(centerAddress)(targetAddress))
  );

  const incomingEdge = pipe(
    trusts,
    G.lookupEdge(ordAddress)(Pair.create(targetAddress)(centerAddress))
  );

  return (
    <tr>
      <td>{getIdentifier(userIdent)}</td>
      <td>
        <pre>
          {pipe(outgoingEdge, x =>
            matchADT(x)({
              Left: () => 'X',
              Right: ([trustConnection]) =>
                matchADT(trustConnection)({
                  TrustConnection: ([_, trustState]) =>
                    matchADT(trustState)({
                      TrustState: ([v]) =>
                        matchV(v)({
                          loadingTrust: () => '',
                          loadingUntrust: () => '',
                          pendingTrust: () => '',
                          pendingUntrust: () => '',
                          trusted: () => '',
                          untrusted: () => '',
                        }),
                    }),
                }),
            })
          )}
          {pipe(outgoingEdge, x =>
            matchADT(x)({
              Left: () => 'X',
              Right: ([trustConnection]) =>
                pipe(trustConnection, fields, ([_, trustState]) =>
                  pipe(trustState, fields, ([v]) =>
                    matchV(v)({
                      loadingTrust: () => '',
                      loadingUntrust: () => '',
                      pendingTrust: () => '',
                      pendingUntrust: () => '',
                      trusted: () => '',
                      untrusted: () => '',
                    })
                  )
                ),
            })
          )}
        </pre>
      </td>
      <td></td>
      <button onClick={() => onAddTrust(userIdent)}>trust</button>
    </tr>
  );
};

// match(

//   )
//   pipe(trustConnection, fields, ([a, trustState]) =>
//     pipe(trustState, ts =>
//       matchADT(ts)({
//         d: 1,
//       })
//     )
//   ),

// -----------------------------------------------------------------------------
// UI UserSearch
// -----------------------------------------------------------------------------

type UserSearchProps = {
  trusts: CirclesGraph;
  userSearchResult: DashboardState['userSearchResult'];
  centerAddress: Address;
  onSearch: (_: string) => void;
  onAddTrust: (_: UserIdent) => void;
};

export const UserSearch = (props: UserSearchProps) => {
  const { userSearchResult, trusts, onSearch } = props;

  const users = getUsers(trusts)(userSearchResult);

  return (
    <div>
      <input type="text" onChange={e => onSearch(e.target.value)} />
      {users.length === 0 ? (
        <div>~ no results ~</div>
      ) : (
        <table>
          {pipe(
            users,
            A.map(userIdent => <Row {...props} userIdent={userIdent} />)
          )}
        </table>
      )}
    </div>
  );
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const getUsers =
  (trusts: CirclesGraph) =>
  (userSearchResult: DashboardState['userSearchResult']) =>
    pipe(
      userSearchResult,
      RD.unwrap,
      remoteData =>
        matchV(remoteData)({
          notAsked: () => [],
          failure: () => [],
          success: ({ data }) => data,
          loading: ({ previousData }) =>
            matchADT(previousData)({
              Just: ([users]) => users,
              Nothing: () => [],
            }),
        }),
      A.map(fromUser)
    );
