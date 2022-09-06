import React, { ReactElement, SetStateAction, useState } from 'react';
import {
  _Tuple,
  _Either,
  _Nullable,
  _IxGraph,
  TrustConnection,
  _TrustState,
  _TrustConnection,
  UserIdent,
  _UserIdent,
  CirclesGraph,
  Address,
  _Address,
  _Array,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { Theme } from '../context/theme';
import { JustifyAroundCenter, JustifyStartCenter, Margin } from './helper';
import { JustText } from './text';
import tw, { css, styled } from 'twin.macro';
import { t } from 'i18next';
import { GridRow } from './GridRow';
import { fetchPageNumbers, paginate } from '../onboarding/utils/paginate';
import { PageSelector } from './PageSelector';
import { LightColorFrame } from './layout';
import { FadeIn, getIncrementor } from 'anima-react';
import ReactTooltip from 'react-tooltip';
import Icon from '@mdi/react';
import {
  mdiAccountArrowLeft,
  mdiAccountArrowRight,
  mdiAccountCancel,
} from '@mdi/js';
import { TrustRow } from './TrustRow';

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

export const USERNAME_WIDTH = 2;
export const RELATION_WIDTH = 1.25;
export const ACTION_WIDTH = 1.25;
export const ROW_HEIGHT = 3;

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type Overlay = 'SEND' | 'RECEIVE' | 'CONFIRM_SEND';

type Props = {
  graph: CirclesGraph;
  address: Address;
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

export type Conn = {
  incoming?: TrustConnection;
  outgoing?: TrustConnection;
};

export const TrustUserList = (props: Props) => {
  // animation
  const getDelay = getIncrementor(0, 0.05);

  const { title, graph, address, theme, icon, actionRow } = props;

  const neighborhood = _IxGraph.neighborhood(_Address.ordAddress)(address)(
    graph
  );
  const items_ = pipe(neighborhood, _Either.hush, _Nullable.toNullable);

  if (!items_) return <div>Address not found in graph!</div>;

  // Paginate trusts
  const [currentPage, setCurrentPage] = useState<number>(1);
  const paginationInfo = paginate(items_.length, currentPage);

  const pageControls = fetchPageNumbers({
    currentPage,
    totalPages: paginationInfo.pages.length,
    pageNeighbours: 1,
  });

  // Get slice on current page
  const items = items_.slice(
    paginationInfo.startIndex,
    paginationInfo.endIndex + 1
  );

  return (
    <LightColorFrame theme={theme} title={title} icon={icon}>
      <>{actionRow}</>
      <>
        {props.description && (
          <Margin top={0.75} bottom={0.75}>
            <JustText fontSize={1.25}>{props.description}</JustText>
          </Margin>
        )}
      </>
      <ListContainer>
        {items.length > 0 && (
          <GridRow
            minHeight={ROW_HEIGHT}
            fields={[
              {
                width: USERNAME_WIDTH,
                content: (
                  <HeadingRowText theme={theme}>
                    {t('dashboard.trustList.tableHead.user')}
                  </HeadingRowText>
                ),
                align: 'LEFT',
              },
              {
                width: RELATION_WIDTH,
                content: (
                  <HeadingRowText theme={theme}>
                    {t('dashboard.trustList.tableHead.relation')}
                  </HeadingRowText>
                ),
                align: 'CENTER',
              },
              {
                width: ACTION_WIDTH,
                content: (
                  <HeadingRowText theme={theme}>
                    {t('dashboard.trustList.tableHead.action')}
                  </HeadingRowText>
                ),
                align: 'CENTER',
              },
            ]}
          />
        )}
        {pipe(
          items,
          _Array.mapArray(x => {
            const [neighborConnectivity, trustNode] = pipe(
              x,
              _Tuple.unTuple(x1 => x2 => [x1, x2])
            );

            const relation = pipe(
              neighborConnectivity,
              _IxGraph.unNeighborConnectivity<TrustConnection, Conn>({
                onJustIncoming: e => ({ incoming: e }),
                onJustOutgoing: e => ({ outgoing: e }),
                onMutualOutAndIn: e1 => e2 => ({ incoming: e1, outgoing: e2 }),
              })
            );

            return (
              <TrustRow
                {...{ relation, trustNode, delay: getDelay() }}
                {...props}
              />
            );
          })
        )}
      </ListContainer>
      <>
        {paginationInfo.totalPages > 1 && (
          <JustifyAroundCenter>
            <PageSelector
              theme={theme}
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
// UI / Table
// -----------------------------------------------------------------------------

export const ListContainer = tw.div`max-w-full border border-gray-100 rounded`;

type HeadingRowTextProps = {
  theme: Theme;
};

export const HeadingRowText = styled.b<HeadingRowTextProps>(
  ({ theme }: HeadingRowTextProps) => [
    tw`lg:text-lg md:text-lg text-left whitespace-nowrap`,
    css`
      color: ${theme.darkColor};
      padding: 0;
      margin: 0;
    `,
  ]
);

// -----------------------------------------------------------------------------
// UI Relation
// -----------------------------------------------------------------------------

type TrustRelationConfig = {
  isTrusted: boolean;
  isUntrusted: boolean;
  pendingTrust: boolean;
  pendingUntrust: boolean;
  loadingTrust: boolean;
  loadingUntrust: boolean;
  isOutgoing: boolean;
};

type RelationProps = {
  trustRelationConfig: TrustRelationConfig;
  getDelay: () => number;
  theme: Theme;
  userIdent: UserIdent;
};

const Relation = ({
  trustRelationConfig,
  getDelay,
  theme,
  userIdent,
}: RelationProps) => {
  const {
    isTrusted,
    isUntrusted,
    pendingTrust,
    pendingUntrust,
    loadingTrust,
    loadingUntrust,
    isOutgoing,
  } = trustRelationConfig;

  return (
    <JustifyAroundCenter>
      <FadeIn orientation={'left'} delay={getDelay()}>
        <>
          <ReactTooltip id="trustlist-relation-from" />
          <Icon
            path={
              isTrusted || pendingUntrust || loadingUntrust
                ? mdiAccountArrowLeft
                : mdiAccountCancel
            }
            size={1.6}
            color={
              isTrusted || pendingUntrust || loadingUntrust
                ? theme.baseColor
                : 'white'
            }
            data-for="trustlist-relation-from"
            data-tip={mapToolTipRelRec(
              isTrusted,
              _UserIdent.getIdentifier(userIdent)
            )}
          />
        </>
      </FadeIn>
      <FadeIn orientation={'left'} delay={getDelay()}>
        <>
          <ReactTooltip id="trustlist-relation-to" />
          <Icon
            path={isOutgoing ? mdiAccountArrowRight : mdiAccountCancel}
            size={1.6}
            color={isOutgoing ? theme.baseColor : 'white'}
            data-for="trustlist-relation-to"
            data-tip={mapToolTipRelSend(
              isOutgoing,
              _UserIdent.getIdentifier(userIdent)
            )}
          />
        </>
      </FadeIn>
    </JustifyAroundCenter>
  );
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------
type ClickableProps = {
  clickable: boolean;
};
export const Clickable = styled.div<ClickableProps>(({ clickable }) => [
  clickable ? tw`cursor-pointer` : tw`cursor-not-allowed`,
]);

type UsernameProps = {
  theme: Theme;
};

export const Username = styled.b<UsernameProps>(({ theme }: UsernameProps) => [
  tw`lg:text-lg md:text-lg text-left whitespace-nowrap`,
  css`
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden;
    color: ${theme.darkColor};
  `,
]);

// -----------------------------------------------------------------------------
// Tooltip mapping
// -----------------------------------------------------------------------------

const replaceUsername = (str: string, username: string) =>
  str.replace(/{{user}}/, username);

const mapToolTipTrust = (trusted: boolean, username: string) => {
  return trusted
    ? replaceUsername(t('dashboard.trustList.untrust'), username)
    : replaceUsername(t('dashboard.trustList.trust'), username);
};

const mapToolTipSend = (sendable: boolean, username: string) => {
  return sendable
    ? replaceUsername(t('dashboard.trustList.send'), username)
    : replaceUsername(t('dashboard.trustList.canNotSend'), username);
};

const mapToolTipRelSend = (sendable: boolean, username: string) => {
  return sendable
    ? replaceUsername(t('dashboard.trustList.relationSendable'), username)
    : replaceUsername(t('dashboard.trustList.relationNotSendable'), username);
};

const mapToolTipRelRec = (receivable: boolean, username: string) => {
  return receivable
    ? replaceUsername(t('dashboard.trustList.relationReceivable'), username)
    : replaceUsername(t('dashboard.trustList.relationNotReceivable'), username);
};
