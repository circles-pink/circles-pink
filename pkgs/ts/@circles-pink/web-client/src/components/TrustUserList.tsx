import {
  Address,
  ordAddress,
} from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import * as G from '@circles-pink/state-machine/output/Data.IxGraph';
import React, { ReactElement, useState } from 'react';
import {
  _Tuple,
  _Either,
  _Nullable,
  _IxGraph,
} from '@circles-pink/state-machine/src';
import * as TN from '@circles-pink/state-machine/output/CirclesPink.Data.TrustNode';
import * as UI from '@circles-pink/state-machine/output/CirclesPink.Data.UserIdent';
import * as A from '@circles-pink/state-machine/output/Simple.Data.Array';
import { CirclesGraph } from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard';
import { pipe } from 'fp-ts/lib/function';
import { Theme } from '../context/theme';
import { JustifyAroundCenter, Margin } from './helper';
import { JustText } from './text';
import tw, { css, styled } from 'twin.macro';
import { t } from 'i18next';
import { GridRow } from './GridRow';
import { fetchPageNumbers, paginate } from '../onboarding/utils/paginate';
import { PageSelector } from './PageSelector';
import { LightColorFrame } from './layout';
import { FadeIn, getIncrementor } from 'anima-react';
import { boolean } from 'fp-ts';

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

export const USERNAME_WIDTH = 2;
export const RELATION_WIDTH = 1.25;
export const ACTION_WIDTH = 1.25;
export const ROW_HEIGHT = 3;

type Props = {
  graph: CirclesGraph;
  address: Address;
  theme: Theme;
  title?: string;
  icon?: string;
  actionRow?: ReactElement | ReactElement[] | string;
  description?: string;
};

export const TrustUserList = (props: Props) => {
  // animation
  const getDelay = getIncrementor(0, 0.05);

  const { title, graph, address, theme, icon, actionRow } = props;

  const neighborhood = G.neighborhood(ordAddress)(address)(graph);
  const items = pipe(neighborhood, _Either.hush, _Nullable.toNullable);

  if (!items) return <div>Address not found in graph!</div>;

  // Paginate trusts
  const [currentPage, setCurrentPage] = useState<number>(1);
  const paginationInfo = paginate(items.length, currentPage);

  const pageControls = fetchPageNumbers({
    currentPage,
    totalPages: paginationInfo.pages.length,
    pageNeighbours: 1,
  });

  // Get slice on current page
  const trusts = items.slice(
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
          A.mapArray(x => {
            console.log(x);

            const [neigborConnectivity, trustNode] = pipe(
              x,
              _Tuple.unTuple(x1 => x2 => [x1, x2])
            );

            const relation = _IxGraph.unNeighborConnectivity({
              onJustIncoming: () => 'incoming',
              onJustOutgoing: () => 'outgoing',
              onMutualOutAndIn: () => () => 'mutual',
            });

            const { userIdent } = TN.unwrap(trustNode);

            const id = UI.getIdentifier(userIdent);

            return <div>{id}</div>;
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

// type RelationProps = {
//   isIncoming: boolean;
//   isOutgoing: boolean;
// }

// const Relation = () => {
//   return (<JustifyAroundCenter>
//     <FadeIn orientation={'left'} delay={getDelay()}>
//       <>
//         <ReactTooltip id="trustlist-relation-from" />
//         <Icon
//           path={
//             isTrusted || pendingUntrust || loadingUntrust
//               ? mdiAccountArrowLeft
//               : mdiAccountCancel
//           }
//           size={1.6}
//           color={
//             isTrusted || pendingUntrust || loadingUntrust
//               ? theme.baseColor
//               : 'white'
//           }
//           data-for="trustlist-relation-from"
//           data-tip={mapToolTipRelRec(isTrusted, userIdent)}
//         />
//       </>
//     </FadeIn>
//     <FadeIn orientation={'left'} delay={getDelay()}>
//       <>
//         <ReactTooltip id="trustlist-relation-to" />
//         <Icon
//           path={
//             c.isOutgoing ? mdiAccountArrowRight : mdiAccountCancel
//           }
//           size={1.6}
//           color={c.isOutgoing ? theme.baseColor : 'white'}
//           data-for="trustlist-relation-to"
//           data-tip={mapToolTipRelSend(c.isOutgoing, userIdent)}
//         />
//       </>
//     </FadeIn>
//   </JustifyAroundCenter>)
// }
