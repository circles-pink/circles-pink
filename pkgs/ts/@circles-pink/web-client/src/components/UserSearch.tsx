import React, {
  ReactElement,
  SetStateAction,
  useEffect,
  useState,
} from 'react';
import tw, { css, styled } from 'twin.macro';
import { Theme } from '../context/theme';
import { Claim, LoadingText } from './text';
import ReactTooltip from 'react-tooltip';
import {
  mdiAccountArrowLeft,
  mdiAccountArrowRight,
  mdiAccountCancel,
  mdiHeart,
  mdiHeartOutline,
  mdiCashFast,
  mdiCashRemove,
  mdiAt,
  mdiWeatherCloudyClock,
} from '@mdi/js';
import Icon from '@mdi/react';
import { darken } from '../onboarding/utils/colorUtils';
import { JustifyAroundCenter, JustifyStartCenter } from './helper';
import { LoadingCircles } from './LoadingCircles';
import {
  addrToString,
  DefaultView,
  Trust,
  Trusts,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import { t } from 'i18next';
import { fetchPageNumbers, paginate } from '../onboarding/utils/paginate';
import { PageSelector } from './PageSelector';
import * as TrustState from '@circles-pink/state-machine/output/CirclesPink.Data.TrustState';
import { FadeIn, getIncrementor } from 'anima-react';
import { User } from '@circles-pink/state-machine/output/CirclesCore';
import { either } from '@circles-pink/state-machine/output/Data.Either';
import { pipe } from 'fp-ts/lib/function';
import {
  getAddress,
  UserIdent,
} from '@circles-pink/state-machine/output/CirclesPink.Data.UserIdent';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { LightColorFrame } from './TrustUserList';

type Overlay = 'SEND' | 'RECEIVE';

type UserSearchProps = {
  title?: string;
  trusts: Trusts;
  theme: Theme;
  icon: any;
  actionRow?: ReactElement | ReactElement[] | string;
  toggleOverlay?: (type: Overlay) => void;
  setOverwriteTo?: React.Dispatch<SetStateAction<Address | undefined>>;
  addTrust: (to: UserIdent) => void;
  removeTrust: (to: UserIdent) => void;
  trustAddResult: DefaultView['trustAddResult'];
  trustRemoveResult: DefaultView['trustRemoveResult'];
};

export const UserSearch = (props: UserSearchProps) => {
  const { title, trusts: allTrusts, theme, icon, actionRow } = props;

  // Paginate trusts
  const [currentPage, setCurrentPage] = useState<number>(1);
  const paginationInfo = paginate(allTrusts.length, currentPage);

  const pageControls = fetchPageNumbers({
    currentPage,
    totalPages: paginationInfo.pages.length,
    pageNeighbours: 1,
  });

  const trusts = [...allTrusts]
    // Sort by username and safeAddress
    .sort((a, b) => {
      const usernameA = pipe(
        a.user,
        either(() => '')(x => (x as User).username)
      );
      const usernameB = pipe(
        b.user,
        either(() => '')(x => (x as User).username)
      );

      const result = usernameA.localeCompare(usernameB);

      if (result !== 0) return result;

      const addressA = pipe(
        a.user,
        either(x => x as Address)(x => (x as User).safeAddress)
      );
      const addressB = pipe(
        b.user,
        either(x => x as Address)(x => (x as User).safeAddress)
      );

      return addrToString(addressA).localeCompare(addrToString(addressB));
    })
    // Get slice on current page
    .slice(paginationInfo.startIndex, paginationInfo.endIndex + 1);

  return (
    <LightColorFrame theme={theme} title={title} icon={icon}>
      <>{actionRow}</>
      <TableContainer>
        <Table>
          {trusts.length > 0 && (
            <TableHeader>
              <TableRow theme={theme}>
                <TableHead>{t('dashboard.trustList.tableHead.user')}</TableHead>
                <TableHead>
                  <JustifyAround>
                    {t('dashboard.trustList.tableHead.relation')}
                  </JustifyAround>
                </TableHead>
                <TableHead>
                  <JustifyAround>
                    {t('dashboard.trustList.tableHead.action')}
                  </JustifyAround>
                </TableHead>
              </TableRow>
            </TableHeader>
          )}

          <TableBody>
            {trusts.map(c => {
              return (
                <ContentRow
                  key={addrToString(getAddress(c.user))}
                  c={c}
                  {...props}
                />
              );
            })}
          </TableBody>
        </Table>
      </TableContainer>
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
// UI / ContentRow
// -----------------------------------------------------------------------------

const ContentRow = (props: UserSearchProps & { c: Trust }): ReactElement => {
  const { c, theme, toggleOverlay, setOverwriteTo, addTrust, removeTrust } =
    props;

  useEffect(() => {
    ReactTooltip.rebuild();
  }, [c]);

  // animation
  const getDelay = getIncrementor(0, 0.05);

  const userIdent = pipe(
    c.user,
    either(x => (x as string).substring(0, 6))(x => (x as User).username)
  );
  const isTrusted = TrustState.isTrusted(c.trustState);
  const isUntrusted = TrustState.isUntrusted(c.trustState);
  const pendingTrust = TrustState.isPendingTrust(c.trustState);
  const pendingUntrust = TrustState.isPendingUntrust(c.trustState);
  const loadingTrust = TrustState.isLoadingTrust(c.trustState);
  const loadingUntrust = TrustState.isLoadingUntrust(c.trustState);
  const inSync = isTrusted || isUntrusted;

  if (inSync) {
    return (
      <TableRow theme={theme}>
        <TableData>
          <FadeIn orientation={'left'} delay={getDelay()}>
            <JustifyStartCenter>
              <Icon path={mdiAt} size={1.5} color={theme.baseColor} />
              <b>{userIdent}</b>
            </JustifyStartCenter>
          </FadeIn>
        </TableData>
        <TableData>
          <ReactTooltip />
          <JustifyAroundCenter>
            <FadeIn orientation={'left'} delay={getDelay()}>
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
                data-tip={mapToolTipRelRec(isTrusted, userIdent)}
              />
            </FadeIn>
            <FadeIn orientation={'left'} delay={getDelay()}>
              <Icon
                path={c.isOutgoing ? mdiAccountArrowRight : mdiAccountCancel}
                size={1.6}
                color={c.isOutgoing ? theme.baseColor : 'white'}
                data-tip={mapToolTipRelSend(c.isOutgoing, userIdent)}
              />
            </FadeIn>
          </JustifyAroundCenter>
        </TableData>
        <TableData>
          <JustifyAroundCenter>
            <FadeIn orientation={'left'} delay={getDelay()}>
              <Clickable
                clickable={c.isOutgoing}
                onClick={() => {
                  if (c.isOutgoing) {
                    if (toggleOverlay && setOverwriteTo) {
                      setOverwriteTo(getAddress(c.user));
                      toggleOverlay('SEND');
                    }
                  }
                }}
              >
                <Icon
                  path={c.isOutgoing ? mdiCashFast : mdiCashRemove}
                  size={1.75}
                  color={c.isOutgoing ? theme.baseColor : 'white'}
                  data-tip={mapToolTipSend(c.isOutgoing, userIdent)}
                />
              </Clickable>
            </FadeIn>

            <FadeIn orientation={'left'} delay={getDelay()}>
              <Clickable
                clickable={true}
                onClick={() => {
                  isUntrusted ? addTrust(c.user) : removeTrust(c.user);
                }}
              >
                <Icon
                  path={isTrusted ? mdiHeart : mdiHeartOutline}
                  size={1.5}
                  color={isTrusted ? theme.baseColor : 'white'}
                  data-tip={mapToolTipTrust(isTrusted, userIdent)}
                />
              </Clickable>
            </FadeIn>
          </JustifyAroundCenter>
        </TableData>
      </TableRow>
    );
  }
  return (
    <TableRow theme={theme}>
      <TableData>
        <FadeIn orientation={'left'} delay={getDelay()}>
          <JustifyStartCenter>
            <Icon path={mdiAt} size={1.5} color={theme.baseColor} />
            <b>{userIdent}</b>
          </JustifyStartCenter>
        </FadeIn>
      </TableData>
      <TableData></TableData>
      <TableData>
        <JustifyAroundCenter>
          <FadeIn orientation={'left'} delay={getDelay()}>
            <TrustActionMessageContainer>
              <TrustActionMessage>
                <LoadingText theme={theme} fontSize={1.6}>
                  {mapStatusMessage(c.trustState)}
                </LoadingText>
              </TrustActionMessage>
            </TrustActionMessageContainer>
          </FadeIn>
          <FadeIn orientation={'left'} delay={getDelay()}>
            <>
              {loadingTrust || loadingUntrust ? (
                <LoadingCircles count={1} width={35} color={theme.baseColor} />
              ) : (
                <Icon
                  path={mdiWeatherCloudyClock}
                  size={1.5}
                  color={theme.baseColor}
                  data-tip={mapToolTipTrust(isTrusted, userIdent)}
                />
              )}
            </>
          </FadeIn>
        </JustifyAroundCenter>
      </TableData>
    </TableRow>
  );
};

// -----------------------------------------------------------------------------
// UI / Frame
// -----------------------------------------------------------------------------

type FameProps = {
  theme: Theme;
};

const Frame = styled.div<FameProps>(({ theme }: FameProps) => [
  tw`block lg:p-8 md:p-8 p-4 border border-gray-800 shadow-xl rounded-xl`,
  css`
    background-color: ${theme.lightColor};
  `,
]);

// -----------------------------------------------------------------------------
// UI / Table
// -----------------------------------------------------------------------------

const TableContainer = tw.div`overflow-hidden overflow-x-auto border border-gray-100 rounded`;
const Table = tw.table`min-w-full text-sm divide-y divide-gray-200`;
const TableHeader = tw.thead`lg:px-4 md:px-4 px-2 lg:text-lg md:text-lg text-left whitespace-nowrap`;
const TableHead = tw.th`lg:px-4 md:px-4 px-2 text-left whitespace-nowrap`;
const TableBody = tw.tbody`divide-y divide-gray-100`;
const TableData = styled.td(() => [
  tw`lg:px-4 md:px-4 px-2 py-2 text-lg whitespace-nowrap`,
  css`
    height: 4.25rem;
  `,
]);

type TableRowProps = {
  theme: Theme;
};
const TableRow = styled.tr<TableRowProps>(({ theme }: TableRowProps) => [
  css`
    color: ${theme.darkColor};
  `,
]);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------
type ClickableProps = {
  clickable: boolean;
};
const Clickable = styled.div<ClickableProps>(({ clickable }) => [
  clickable ? tw`cursor-pointer` : tw`cursor-not-allowed`,
]);

const Title = tw.div`mb-4`;
const JustifyBetween = tw.div`flex justify-between`;
const JustifyAround = tw.div`flex justify-around`;
const TrustActionMessageContainer = tw.div`relative w-6 h-6`;
const TrustActionMessage = tw.span`absolute right-0 top-0`;

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const mapStatusMessage = (trustState: TrustState.TrustState) => {
  if (TrustState.isLoadingTrust(trustState)) {
    return t('dashboard.trustList.message.loadingTrust');
  } else if (TrustState.isLoadingUntrust(trustState)) {
    return t('dashboard.trustList.message.loadingUntrust');
  } else if (TrustState.isPendingTrust(trustState)) {
    return t('dashboard.trustList.message.pendingTrust');
  } else if (TrustState.isPendingUntrust(trustState)) {
    return t('dashboard.trustList.message.pendingUntrust');
  }
};

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
