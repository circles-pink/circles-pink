import { pipe } from 'fp-ts/lib/function';
import React from 'react';
import { fields, matchADT, matchV } from '../purs-util';
import {
  Address,
  CirclesGraph,
  DashboardState,
  UserIdent,
  _Address,
  _Array,
  _ArrayS,
  _IxGraph,
  _Pair,
  _RemoteData,
  _TrustState,
  _UserIdent,
} from '@circles-pink/state-machine/src';

// -----------------------------------------------------------------------------
// UI / Row
// -----------------------------------------------------------------------------

type RowProps = { userIdent: UserIdent } & UserSearchProps;

const Row = ({ centerAddress, userIdent, trusts, onAddTrust }: RowProps) => {
  const targetAddress = _UserIdent.getAddress(userIdent);

  const outgoingEdge = pipe(
    trusts,
    _IxGraph.lookupEdge(_Address.ordAddress)(
      _Pair.Pair.create(centerAddress)(targetAddress)
    )
  );

  const incomingEdge = pipe(
    trusts,
    _IxGraph.lookupEdge(_Address.ordAddress)(
      _Pair.Pair.create(targetAddress)(centerAddress)
    )
  );

  return (
    <tr>
      <td>{_UserIdent.getIdentifier(userIdent)}</td>
      <td>
        <pre>
          {matchADT(outgoingEdge)({
            Left: () => '  X',
            Right: ([trustConnection]) => {
              const [_, trustState] = fields(trustConnection);
              const trustState_ = _TrustState.unwrap(trustState);

              return matchV(trustState_)({
                loadingTrust: () => '..O',
                loadingUntrust: () => '..X',
                pendingTrust: () => ' .O',
                pendingUntrust: () => ' .X',
                trusted: () => '  O',
                untrusted: () => '  X',
              });
            },
          })}
        </pre>
      </td>
      <td>
        {matchADT(outgoingEdge)({
          Left: () => '  X',
          Right: ([trustConnection]) => {
            const [_, trustState] = fields(trustConnection);
            const trustState_ = _TrustState.unwrap(trustState);

            return matchV(trustState_)({
              loadingTrust: () => '%',
              loadingUntrust: () => '%',
              pendingTrust: () => ' .%',
              pendingUntrust: () => '%',
              trusted: () => 'O',
              untrusted: () => 'X',
            }, () => "%");
          },
        })}
      </td>
      <button onClick={() => onAddTrust(userIdent)}>trust</button>
    </tr>
  );
};

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
  const { userSearchResult, onSearch } = props;

  const users = getUsers(userSearchResult);

  return (
    <div>
      <input type="text" onChange={e => onSearch(e.target.value)} />
      {users.length === 0 ? (
        <div>~ no results ~</div>
      ) : (
        <table>
          {pipe(
            users,
            _ArrayS.map(userIdent => <Row {...props} userIdent={userIdent} />)
          )}
        </table>
      )}
    </div>
  );
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const getUsers = (userSearchResult: DashboardState['userSearchResult']) => {
  const userSearchResult_ = _RemoteData.unwrap(userSearchResult);

  const users = matchV(userSearchResult_)({
    notAsked: () => [],
    failure: () => [],
    success: ({ data }) => data,
    loading: ({ previousData }) =>
      matchADT(previousData)({
        Just: ([users]) => users,
        Nothing: () => [],
      }),
  });

  return _ArrayS.map(_UserIdent.fromUser)(users);
};
