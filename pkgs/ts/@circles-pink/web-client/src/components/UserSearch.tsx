import React, {
  ReactElement,
  SetStateAction,
  useEffect,
  useState,
} from 'react';
import tw, { css, styled } from 'twin.macro';
import { Theme } from '../context/theme';
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
import { JustifyAroundCenter, JustifyStartCenter } from './helper';
import { LoadingCircles } from './LoadingCircles';
import {
  addrToString,
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
import { TrustStatusMessage } from './TrustStatusMessage';
import { LightColorFrame } from './layout';
import {
  ACTION_WIDTH,
  Clickable,
  HeadingRowText,
  ListContainer,
  RELATION_WIDTH,
  ROW_HEIGHT,
  Username,
  USERNAME_WIDTH,
} from './TrustUserList';
import { GridRow } from './GridRow';

type Overlay = 'SEND' | 'RECEIVE';

type UserSearchProps = {
  title?: string;
  trusts: Trusts;
  ownSafeAddress: Address;
  theme: Theme;
  icon: any;
  actionRow?: ReactElement | ReactElement[] | string;
  toggleOverlay?: (type: Overlay) => void;
  setOverwriteTo?: React.Dispatch<SetStateAction<Address | undefined>>;
  addTrust: (to: UserIdent) => void;
  removeTrust: (to: UserIdent) => void;
};

export const UserSearch = (props: UserSearchProps) => {
  const {
    title,
    trusts: allTrusts,
    ownSafeAddress,
    theme,
    icon,
    actionRow,
  } = props;

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
    // Filter out own user
    .filter((t: Trust) => getAddress(t.user) !== ownSafeAddress)
    // Get slice on current page
    .slice(paginationInfo.startIndex, paginationInfo.endIndex + 1);

  return (
    <LightColorFrame theme={theme} title={title} icon={icon}>
      <>{actionRow}</>
      <ListContainer>
        {trusts.length > 0 && (
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

        {trusts.map(c => {
          return (
            <ContentRow
              key={addrToString(getAddress(c.user))}
              c={c}
              {...props}
            />
          );
        })}
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
      <GridRow
        minHeight={ROW_HEIGHT}
        fields={[
          {
            width: USERNAME_WIDTH,
            content: (
              <FadeIn orientation={'left'} delay={getDelay()}>
                <JustifyStartCenter>
                  <ReactTooltip id="username" />
                  <div>
                    <Icon path={mdiAt} size={1.25} color={theme.baseColor} />
                  </div>
                  <Username
                    theme={theme}
                    data-tip={userIdent}
                    data-for="username"
                  >
                    {userIdent}
                  </Username>
                </JustifyStartCenter>
              </FadeIn>
            ),
            align: 'LEFT',
          },
          {
            width: RELATION_WIDTH,
            content: (
              <JustifyAroundCenter>
                <FadeIn orientation={'left'} delay={getDelay()}>
                  <>
                    <ReactTooltip id="relation-from" />
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
                      data-for="relation-from"
                      data-tip={mapToolTipRelRec(isTrusted, userIdent)}
                    />
                  </>
                </FadeIn>
                <FadeIn orientation={'left'} delay={getDelay()}>
                  <>
                    <ReactTooltip id="relation-to" />
                    <Icon
                      path={
                        c.isOutgoing ? mdiAccountArrowRight : mdiAccountCancel
                      }
                      size={1.6}
                      color={c.isOutgoing ? theme.baseColor : 'white'}
                      data-for="relation-to"
                      data-tip={mapToolTipRelRec(isTrusted, userIdent)}
                    />
                  </>
                </FadeIn>
              </JustifyAroundCenter>
            ),
            align: 'CENTER',
          },
          {
            width: ACTION_WIDTH,
            content: (
              <JustifyAroundCenter>
                <FadeIn orientation={'left'} delay={getDelay()}>
                  <>
                    <ReactTooltip id="action-send" />
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
                        data-for="action-send"
                        data-tip={mapToolTipSend(c.isOutgoing, userIdent)}
                      />
                    </Clickable>
                  </>
                </FadeIn>
                <FadeIn orientation={'left'} delay={getDelay()}>
                  <>
                    <ReactTooltip id="action-trust" />
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
                        data-for="action-trust"
                        data-tip={mapToolTipTrust(isTrusted, userIdent)}
                      />
                    </Clickable>
                  </>
                </FadeIn>
              </JustifyAroundCenter>
            ),
            align: 'CENTER',
          },
        ]}
      />
    );
  }
  return (
    <GridRow
      minHeight={ROW_HEIGHT}
      fields={[
        { width: 0, align: 'LEFT', content: <ReactTooltip /> },
        {
          width: USERNAME_WIDTH,
          content: (
            <FadeIn orientation={'left'} delay={getDelay()}>
              <JustifyStartCenter>
                <ReactTooltip id="username" />
                <div>
                  <Icon path={mdiAt} size={1.25} color={theme.baseColor} />
                </div>
                <Username
                  theme={theme}
                  data-tip={userIdent}
                  data-for="username"
                >
                  {userIdent}
                </Username>
              </JustifyStartCenter>
            </FadeIn>
          ),
          align: 'LEFT',
        },
        {
          width: RELATION_WIDTH * 2,
          content: (
            <TrustStatusMessage theme={theme} trustState={c.trustState} />
          ),
          align: 'RIGHT',
        },
        {
          width: ACTION_WIDTH / 2,
          content: (
            <FadeIn orientation={'left'} delay={getDelay()}>
              <>
                <ReactTooltip id="action" />
                {loadingTrust || loadingUntrust ? (
                  <LoadingCircles
                    count={1}
                    width={35}
                    color={theme.baseColor}
                  />
                ) : (
                  <Icon
                    path={mdiWeatherCloudyClock}
                    size={1.5}
                    color={theme.baseColor}
                    data-tip={mapToolTipTrust(isTrusted, userIdent)}
                    data-for="action"
                  />
                )}
              </>
            </FadeIn>
          ),
          align: 'LEFT',
        },
      ]}
    />
  );
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
