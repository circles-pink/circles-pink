import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import React, { SetStateAction, useEffect, useState } from 'react';
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
  _TrustNode,
  TrustNode,
  DashboardState,
  _Address,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { Theme } from '../../context/theme';
import { JustifyAroundCenter, JustifyStartCenter, Margin } from '../helper';
import tw, { css, styled } from 'twin.macro';
import { t } from 'i18next';
import { GridRow } from '../GridRow';
import { FadeIn, getIncrementor } from 'anima-react';
import ReactTooltip from 'react-tooltip';
import Icon from '@mdi/react';
import {
  mdiAccountArrowLeft,
  mdiAccountArrowRight,
  mdiAccountCancel,
  mdiAt,
  mdiCashFast,
  mdiCashRemove,
  mdiHeart,
  mdiHeartOutline,
  mdiWeatherCloudyClock,
} from '@mdi/js';
import { LoadingCircles } from '../LoadingCircles';
import { TrustStatusMessage } from './TrustStatusMessage';
import { useTrustResult } from './hooks/useTrustResult';

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

export const USERNAME_WIDTH = 2;
export const RELATION_WIDTH = 1.25;
export const ACTION_WIDTH = 1.25;
export const ROW_HEIGHT = 3;

const tooltipIds = (prefix: string) => ({
  usernameInSync: `${prefix}-usernameInSync`,
  actionSendInSync: `${prefix}-actionSendInSync`,
  actionTrustInSync: `${prefix}-actionTrustInSync`,
  usernameNotInSync: `${prefix}-usernameNotInSync`,
  actionNotInSync: `${prefix}-actionNotInSync`,
  relationFrom: `${prefix}-relationFrom`,
  relationTo: `${prefix}-relationTo`,
});

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type Overlay = 'SEND' | 'RECEIVE' | 'CONFIRM_SEND';

type Conn = {
  incoming?: TrustConnection;
  outgoing?: TrustConnection;
};

type TrustRowProps = {
  relation: Conn;
  trustNode: TrustNode;
  delay: number;
  theme: Theme;
  toggleOverlay?: (type: Overlay) => void;
  setOverwriteTo?: React.Dispatch<SetStateAction<Address | undefined>>;
  addTrust: (to: UserIdent) => void;
  removeTrust: (to: UserIdent) => void;
  trustAddResult: DashboardState['trustAddResult'];
  trustRemoveResult: DashboardState['trustRemoveResult'];
};

export const TrustRow = (props: TrustRowProps) => {
  const {
    relation,
    trustNode,
    delay,
    theme,
    toggleOverlay,
    setOverwriteTo,
    addTrust,
    removeTrust,
    trustAddResult,
    trustRemoveResult,
  } = props;

  const getDelay = getIncrementor(0, 0.05);

  const [tooltipId] = useState(Math.random().toString());

  const trustConnection = relation.incoming;

  const trustState = (() => {
    if (trustConnection) {
      return pipe(
        trustConnection,
        _TrustConnection.unTrustConnection(() => r => r)
      );
    } else {
      return _TrustState.initUntrusted;
    }
  })();

  // const trustState_ = _TrustState.unTrustState(trustState);

  const trustRelationConfig: TrustRelationConfig = {
    isTrusted: _TrustState.isTrusted(trustState),
    isUntrusted: _TrustState.isUntrusted(trustState),
    pendingTrust: _TrustState.isPendingTrust(trustState),
    pendingUntrust: _TrustState.isPendingUntrust(trustState),
    loadingTrust: _TrustState.isLoadingTrust(trustState),
    loadingUntrust: _TrustState.isLoadingUntrust(trustState),
    isOutgoing: !!relation.outgoing,
  };

  const inSync =
    trustRelationConfig.isTrusted || trustRelationConfig.isUntrusted;

  const { userIdent } = trustNode;

  const address = _Address.addrToString(_UserIdent.getAddress(userIdent));

  const trustResult = useTrustResult(
    address,
    trustAddResult,
    trustRemoveResult
  );

  if (inSync) {
    return (
      <GridRow
        minHeight={ROW_HEIGHT}
        fields={[
          {
            width: USERNAME_WIDTH,
            content: (
              <FadeIn orientation={'left'} delay={delay}>
                <JustifyStartCenter>
                  <ReactTooltip id={tooltipIds(tooltipId).usernameInSync} />
                  <div>
                    <Icon path={mdiAt} size={1.25} color={theme.baseColor} />
                  </div>
                  <Username
                    theme={theme}
                    data-for={tooltipIds(tooltipId).usernameInSync}
                    data-tip={_UserIdent.getIdentifier(userIdent)}
                  >
                    {_UserIdent.getIdentifier(userIdent)}
                  </Username>
                </JustifyStartCenter>
              </FadeIn>
            ),
            align: 'LEFT',
          },
          {
            width: RELATION_WIDTH,
            content: (
              <Relation
                trustRelationConfig={trustRelationConfig}
                getDelay={getDelay}
                theme={theme}
                userIdent={userIdent}
              />
            ),
            align: 'CENTER',
          },
          {
            width: ACTION_WIDTH,
            content: (
              <JustifyAroundCenter>
                <FadeIn orientation={'left'} delay={delay}>
                  <>
                    <ReactTooltip id={tooltipIds(tooltipId).actionSendInSync} />
                    <Clickable
                      clickable={trustRelationConfig.isOutgoing}
                      onClick={() => {
                        if (
                          trustRelationConfig.isOutgoing &&
                          toggleOverlay &&
                          setOverwriteTo
                        ) {
                          setOverwriteTo(_UserIdent.getAddress(userIdent));
                          toggleOverlay('SEND');
                        }
                      }}
                    >
                      <Icon
                        path={
                          trustRelationConfig.isOutgoing
                            ? mdiCashFast
                            : mdiCashRemove
                        }
                        size={1.75}
                        color={
                          trustRelationConfig.isOutgoing
                            ? theme.baseColor
                            : 'white'
                        }
                        data-for={tooltipIds(tooltipId).actionSendInSync}
                        data-tip={mapToolTipSend(
                          trustRelationConfig.isOutgoing,
                          _UserIdent.getIdentifier(userIdent)
                        )}
                      />
                    </Clickable>
                  </>
                </FadeIn>
                <FadeIn orientation={'left'} delay={delay}>
                  <>
                    <ReactTooltip
                      id={tooltipIds(tooltipId).actionTrustInSync}
                    />
                    <Clickable
                      clickable={true}
                      onClick={() => {
                        trustRelationConfig.isUntrusted
                          ? addTrust(userIdent)
                          : removeTrust(userIdent);
                      }}
                    >
                      <Icon
                        path={
                          trustRelationConfig.isTrusted
                            ? mdiHeart
                            : mdiHeartOutline
                        }
                        size={1.5}
                        color={
                          trustRelationConfig.isTrusted
                            ? theme.baseColor
                            : 'white'
                        }
                        data-for={tooltipIds(tooltipId).actionTrustInSync}
                        data-tip={mapToolTipTrust(
                          trustRelationConfig.isTrusted,
                          _UserIdent.getIdentifier(userIdent)
                        )}
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
        {
          width: USERNAME_WIDTH,
          content: (
            <FadeIn orientation={'left'} delay={delay}>
              <JustifyStartCenter>
                <ReactTooltip id={tooltipIds(tooltipId).usernameNotInSync} />
                <div>
                  <Icon path={mdiAt} size={1.25} color={theme.baseColor} />
                </div>
                <Username
                  theme={theme}
                  data-tip={_UserIdent.getIdentifier(userIdent)}
                  data-for={tooltipIds(tooltipId).usernameNotInSync}
                >
                  {_UserIdent.getIdentifier(userIdent)}
                </Username>
              </JustifyStartCenter>
            </FadeIn>
          ),
          align: 'LEFT',
        },
        {
          width: RELATION_WIDTH * 2,
          content: (
            <>
              {trustResult?.lastState !== 'failed' ? (
                <TrustStatusMessage
                  theme={theme}
                  trustState={trustState}
                  trustAddResult={trustAddResult}
                  trustRemoveResult={trustRemoveResult}
                  userIdent={userIdent}
                />
              ) : (
                <></>
              )}
            </>
          ),

          align: 'RIGHT',
        },
        {
          width: ACTION_WIDTH / 2,
          content: (
            <FadeIn orientation={'left'} delay={delay}>
              <>
                <ReactTooltip id={tooltipIds(tooltipId).actionNotInSync} />
                {trustRelationConfig.loadingTrust ||
                trustRelationConfig.loadingUntrust ? (
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
                    data-for={tooltipIds(tooltipId).actionNotInSync}
                    data-tip={mapToolTipTrust(
                      trustRelationConfig.isTrusted,
                      _UserIdent.getIdentifier(userIdent)
                    )}
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

  const [tooltipId] = useState(Math.random().toString());

  return (
    <JustifyAroundCenter>
      <FadeIn orientation={'left'} delay={getDelay()}>
        <>
          <ReactTooltip id={tooltipIds(tooltipId).relationFrom} />
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
            data-for={tooltipIds(tooltipId).relationFrom}
            data-tip={mapToolTipRelRec(
              isTrusted,
              _UserIdent.getIdentifier(userIdent)
            )}
          />
        </>
      </FadeIn>
      <FadeIn orientation={'left'} delay={getDelay()}>
        <>
          <ReactTooltip id={tooltipIds(tooltipId).relationTo} />
          <Icon
            path={isOutgoing ? mdiAccountArrowRight : mdiAccountCancel}
            size={1.6}
            color={isOutgoing ? theme.baseColor : 'white'}
            data-for={tooltipIds(tooltipId).relationTo}
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
