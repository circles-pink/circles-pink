import { pipe } from 'fp-ts/lib/function';
import React, { ReactElement, SetStateAction, useState } from 'react';
import { isCaseV, matchV } from '../purs-util';
import {
  Address,
  CirclesGraph,
  DashboardState,
  mapArray,
  Maybe,
  Pair,
  TrustConnection,
  TrustNode,
  unTrustState,
  UserIdent,
  _Address,
  _IxGraph,
  _Maybe,
  _Nullable,
  _Pair,
  _RemoteData,
  _TrustConnection,
  _TrustNode,
  _TrustState,
  _UserIdent,
} from '@circles-pink/state-machine/src';
import { hush } from '@circles-pink/state-machine/output/Data.Either';
import {
  ACTION_WIDTH,
  HeadingRowText,
  ListContainer,
  RELATION_WIDTH,
  ROW_HEIGHT,
  TrustRow,
  USERNAME_WIDTH,
} from './TrustRow';
import { Theme } from '../context/theme';
import { getIncrementor } from 'anima-react';
import { Conn } from './TrustUserList';
import { ordAddress } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { LightColorFrame } from './layout';
import { JustifyAroundCenter, Margin } from './helper';
import { JustText } from './text';
import { GridRow } from './GridRow';
import { t } from 'i18next';
import { fetchPageNumbers, paginate } from '../onboarding/utils/paginate';
import { PageSelector } from './PageSelector';

// -----------------------------------------------------------------------------
// UI / Row
// -----------------------------------------------------------------------------

type RowProps = {
  userIdent: UserIdent;
  getDelay: () => number;
} & UserSearchProps;

const Row = (props: RowProps) => {
  const { centerAddress, userIdent, trusts, onAddTrust } = props;

  const targetAddress = _UserIdent.getAddress(userIdent);

  const outgoingEdge = pipe(
    trusts,
    lookupEdge(Pair.create(centerAddress)(targetAddress)),
    hush,
    _Nullable.toNullable
  );

  const incomingEdge = pipe(
    trusts,
    lookupEdge(Pair.create(targetAddress)(centerAddress)),
    hush,
    _Nullable.toNullable
  );

  const relation: Conn = {
    incoming: outgoingEdge || undefined,
    outgoing: incomingEdge || undefined,
  };

  const trustNode: TrustNode = pipe(
    trusts,
    _IxGraph.lookupNode(ordAddress)(targetAddress),
    hush,
    _Maybe.unMaybe({
      onJust: tn => tn,
      onNothing: () => _TrustNode.initTrustNode(userIdent),
    })
  );

  return (
    <TrustRow
      {...{ relation, trustNode, delay: props.getDelay() }}
      {...props}
    />
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

type Overlay = 'SEND' | 'RECEIVE' | 'CONFIRM_SEND';

type UserSearchProps = {
  trusts: CirclesGraph;
  userSearchResult: DashboardState['userSearchResult'];
  centerAddress: Address;
  onSearch: (_: string) => void;
  onAddTrust: (_: UserIdent) => void;
  theme: Theme;
  title?: string;
  icon?: string;
  actionRow?: ReactElement | ReactElement[] | string;
  description?: string;
  toggleOverlay?: (type: Overlay) => void;
  setOverwriteTo?: React.Dispatch<SetStateAction<Address | undefined>>;
  addTrust: (to: UserIdent) => void;
  removeTrust: (to: UserIdent) => void;
};

export const UserSearch = (props: UserSearchProps) => {
  const { userSearchResult, onSearch } = props;

  // animation
  const getDelay = getIncrementor(0, 0.05);

  const users_ = getUsers(userSearchResult);

  // Paginate trusts
  const [currentPage, setCurrentPage] = useState<number>(1);
  const paginationInfo = paginate(users_.length, currentPage);

  const pageControls = fetchPageNumbers({
    currentPage,
    totalPages: paginationInfo.pages.length,
    pageNeighbours: 1,
  });

  // Get slice on current page
  const users = users_.slice(
    paginationInfo.startIndex,
    paginationInfo.endIndex + 1
  );

  return (
    <LightColorFrame theme={props.theme} title={props.title} icon={props.icon}>
      <>{props.actionRow}</>
      <>
        {props.description && (
          <Margin top={0.75} bottom={0.75}>
            <JustText fontSize={1.25}>{props.description}</JustText>
          </Margin>
        )}
      </>
      <ListContainer>
        {users.length > 0 && (
          <GridRow
            minHeight={ROW_HEIGHT}
            fields={[
              {
                width: USERNAME_WIDTH,
                content: (
                  <HeadingRowText theme={props.theme}>
                    {t('dashboard.trustList.tableHead.user')}
                  </HeadingRowText>
                ),
                align: 'LEFT',
              },
              {
                width: RELATION_WIDTH,
                content: (
                  <HeadingRowText theme={props.theme}>
                    {t('dashboard.trustList.tableHead.relation')}
                  </HeadingRowText>
                ),
                align: 'CENTER',
              },
              {
                width: ACTION_WIDTH,
                content: (
                  <HeadingRowText theme={props.theme}>
                    {t('dashboard.trustList.tableHead.action')}
                  </HeadingRowText>
                ),
                align: 'CENTER',
              },
            ]}
          />
        )}
        {pipe(
          users,
          mapArray(userIdent => (
            <Row {...props} userIdent={userIdent} getDelay={getDelay} />
          ))
        )}
      </ListContainer>
      <>
        {paginationInfo.totalPages > 1 && (
          <JustifyAroundCenter>
            <PageSelector
              theme={props.theme}
              currentPage={currentPage}
              setCurrentPage={setCurrentPage}
              pageControls={pageControls}
            />
          </JustifyAroundCenter>
        )}
      </>
    </LightColorFrame>
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
      pipe(
        previousData,
        _Maybe.unMaybe({
          onJust: users => users,
          onNothing: () => [],
        })
      ),
  });

  return mapArray(_UserIdent.fromUser)(users);
};

const isTrusting = (tc: Maybe<TrustConnection>): boolean => {
  const _tc = pipe(tc, _Nullable.toNullable);
  if (!_tc) return false;

  const trustState = pipe(
    _tc,
    _TrustConnection.unTrustConnection(() => r => r)
  );
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
