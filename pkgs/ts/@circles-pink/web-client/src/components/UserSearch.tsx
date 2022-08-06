import { pipe } from 'fp-ts/lib/function';
import React from 'react';
import { fieldsOf, fieldOf, isCaseV, matchADT, matchV, isCase, run } from '../purs-util';
import {
  Address,
  CirclesGraph,
  DashboardState,
  unTrustState,
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
import { hush, isLeft } from '@circles-pink/state-machine/output/Data.Either';

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
    ),
    hush
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
          {/* {matchADT(outgoingEdge)({
            Left: () => '  X',
            Right: ([trustConnection]) => {
              const [_, trustState] =
                fields('TrustConnection')(trustConnection);
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
          })} */}
        </pre>
      </td>

      <td>
        {run(() => {
          if (isCase('Nothing')(outgoingEdge)) return <NotTrusting />;

          const [trustConnection] = fieldsOf('Just')(outgoingEdge);
          
          const [_, trustState] = fieldsOf('TrustConnection')(trustConnection);

          if (isCaseV('trusted')(unTrustState(trustState)))
            return <NotTrusting />;

          return <Trusting />;
        })}
      </td>

      <button onClick={() => onAddTrust(userIdent)}>trust</button>
    </tr>
  );
};

const Trusting = () => null;

const NotTrusting = () => null;

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
