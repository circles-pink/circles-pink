import { pipe } from 'fp-ts/lib/function';
import React from 'react';
import { fieldsOf, isCaseV, matchADT, matchV, isCase } from '../purs-util';
import {
  Address,
  CirclesGraph,
  DashboardState,
  mapArray,
  Maybe,
  Pair,
  TrustConnection,
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

const Row = (props: RowProps) => {
  const { centerAddress, userIdent, trusts, onAddTrust } = props;

  const targetAddress = _UserIdent.getAddress(userIdent);

  const outgoingEdge = pipe(
    trusts,
    lookupEdge(Pair.create(centerAddress)(targetAddress)),
    hush
  );

  const incomingEdge = pipe(
    trusts,
    lookupEdge(Pair.create(targetAddress)(centerAddress)),
    hush
  );

  return (
    <tr>
      <td>{_UserIdent.getIdentifier(userIdent)}</td>
      <td>
        {isTrusting(outgoingEdge) ? (
          <LoadingIndicator {...props} {...{ outgoingEdge }} />
        ) : (
          <TrustRelations {...props} {...{ outgoingEdge, incomingEdge }} />
        )}
      </td>
      <td>
        <Heart {...props} {...{ outgoingEdge }} />
      </td>
      <td>
        <button onClick={() => onAddTrust(userIdent)}>trust</button>
      </td>
    </tr>
  );
};

// -----------------------------------------------------------------------------
// UI LoadingIndicator
// -----------------------------------------------------------------------------

type LoadingIndicatorProps = RowProps & {
  outgoingEdge: Maybe<TrustConnection>;
};

const LoadingIndicator = (props: LoadingIndicatorProps) => null;

// -----------------------------------------------------------------------------
// UI TrustRelations
// -----------------------------------------------------------------------------

type TrustRelationsProps = RowProps & {
  incomingEdge: Maybe<TrustConnection>;
  outgoingEdge: Maybe<TrustConnection>;
};

const TrustRelations = (props: TrustRelationsProps) => null;

// -----------------------------------------------------------------------------
// UI Heart
// -----------------------------------------------------------------------------

type HeartProps = RowProps & {
  outgoingEdge: Maybe<TrustConnection>;
};

const Heart = (props: HeartProps) => null;

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
            mapArray(userIdent => <Row {...props} userIdent={userIdent} />)
          )}
        </table>
      )}
    </div>
  );
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const getUsers = (
  userSearchResult: DashboardState['userSearchResult']
): readonly UserIdent[] => {
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

  return mapArray(_UserIdent.fromUser)(users);
};

const isTrusting = (tc: Maybe<TrustConnection>): boolean => {
  if (isCase('Nothing')(tc)) return false;

  const [trustConnection] = fieldsOf('Just')(tc);
  const [_, trustState] = fieldsOf('TrustConnection')(trustConnection);
  const trustState_ = unTrustState(trustState);

  if (isCaseV('trusted')(trustState_)) return false;

  return true;
};

const lookupEdge = _IxGraph.lookupEdge(_Address.ordAddress);

// <td>
// <pre>
//   {/* {matchADT(outgoingEdge)({
//     Left: () => '  X',
//     Right: ([trustConnection]) => {
//       const [_, trustState] =
//         fields('TrustConnection')(trustConnection);
//       const trustState_ = _TrustState.unwrap(trustState);

//       return matchV(trustState_)({
//         loadingTrust: () => '..O',
//         loadingUntrust: () => '..X',
//         pendingTrust: () => ' .O',
//         pendingUntrust: () => ' .X',
//         trusted: () => '  O',
//         untrusted: () => '  X',
//       });
//     },
//   })} */}
// </pre>
// </td>
