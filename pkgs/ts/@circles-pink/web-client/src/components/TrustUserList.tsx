import React, { ReactElement, SetStateAction, useEffect } from 'react';
import tw, { css, styled } from 'twin.macro';
import { Theme } from '../context/theme';
import { Claim } from './text';
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
  mdiTimerSand,
} from '@mdi/js';
import Icon from '@mdi/react';
import { darken } from '../onboarding/utils/colorUtils';
import { addrToString } from '@circles-pink/state-machine/output/Wallet.PrivateKey';
import { JustifyBetweenCenter, JustifyStartCenter } from './helper';
import { RemoteReport } from '@circles-pink/state-machine/output/RemoteReport';
import { LoadingCircles } from './LoadingCircles';
import {
  DefaultView,
  ErrTrustAddConnectionResolved,
  Trust,
  Trusts,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import { t } from 'i18next';

type Overlay = 'SEND' | 'RECEIVE';

type TrustUserListProps = {
  title?: string;
  trusts: Trusts;
  theme: Theme;
  icon: any;
  actionRow?: ReactElement | ReactElement[] | string;
  toggleOverlay?: (type: Overlay) => void;
  setOverwriteTo?: React.Dispatch<SetStateAction<string>>;
  addTrust: (to: string) => void;
  removeTrust: (to: string) => void;
  trustAddResult: DefaultView['trustAddResult'];
  trustRemoveResult: DefaultView['trustRemoveResult'];
};

export const TrustUserList = (props: TrustUserListProps) => {
  const { title, trusts, theme, icon, actionRow } = props;

  return (
    <Frame theme={theme}>
      <Title>
        <JustifyBetween>
          <Claim color={darken(theme.lightColor, 2)}>{title}</Claim>
          <Icon path={icon} size={1.5} color={darken(theme.lightColor, 2)} />
        </JustifyBetween>
      </Title>
      {actionRow}
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
            {trusts.map((c, index) => {
              return (
                <ContentRow
                  key={addrToString(c.safeAddress)}
                  c={c}
                  {...props}
                />
              );
            })}
          </TableBody>
        </Table>
      </TableContainer>
    </Frame>
  );
};

// -----------------------------------------------------------------------------
// UI / ContentRow
// -----------------------------------------------------------------------------

const ContentRow = (props: TrustUserListProps & { c: Trust }): ReactElement => {
  const { c, theme, toggleOverlay, setOverwriteTo, addTrust, removeTrust } =
    props;

  useEffect(() => {
    ReactTooltip.rebuild();
  }, [c]);

  const userIdent = c.user ? c.user.username : c.safeAddress.substring(0, 6);

  return (
    <TableRow theme={theme}>
      <TableData>
        <JustifyStartCenter>
          <Icon path={mdiAt} size={1.5} color={theme.baseColor} />
          <b>{userIdent}</b>
        </JustifyStartCenter>
      </TableData>
      <TableData>
        <ReactTooltip />
        <JustifyBetweenCenter>
          <Icon
            path={c.isIncoming ? mdiAccountArrowLeft : mdiAccountCancel}
            size={1.6}
            color={c.isIncoming ? theme.baseColor : 'white'}
            data-tip={mapToolTipRelRec(c.isIncoming, userIdent)}
          />
          <Icon
            path={c.isOutgoing ? mdiAccountArrowRight : mdiAccountCancel}
            size={1.6}
            color={c.isOutgoing ? theme.baseColor : 'white'}
            data-tip={mapToolTipRelSend(c.isOutgoing, userIdent)}
          />
        </JustifyBetweenCenter>
      </TableData>
      <TableData>
        <JustifyBetweenCenter>
          <Clickable
            clickable={c.isOutgoing}
            onClick={() => {
              if (c.isOutgoing) {
                if (toggleOverlay && setOverwriteTo) {
                  setOverwriteTo(addrToString(c.safeAddress));
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

          {c.trustState.type === 'inSync' ? (
            <Clickable
              clickable={true}
              onClick={() => {
                !c.isIncoming
                  ? addTrust(addrToString(c.safeAddress))
                  : removeTrust(addrToString(c.safeAddress));
              }}
            >
              <Icon
                path={c.isIncoming ? mdiHeart : mdiHeartOutline}
                size={1.5}
                color={c.isIncoming ? theme.baseColor : 'white'}
                data-tip={mapToolTipTrust(c.isIncoming, userIdent)}
              />
            </Clickable>
          ) : (
            <>
              {c.trustState.type === 'loadingTrust' ||
              c.trustState.type === 'loadingUntrust' ? (
                <LoadingCircles count={1} width={35} color={theme.baseColor} />
              ) : (
                <Icon
                  path={mdiTimerSand}
                  size={1.5}
                  color={theme.baseColor}
                  data-tip={mapToolTipTrust(c.isIncoming, userIdent)}
                />
              )}
            </>
          )}
        </JustifyBetweenCenter>
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
const TableData = tw.td`lg:px-4 md:px-4 px-2 py-2 text-lg whitespace-nowrap`;

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

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const trustIsLoading = (
  result: RemoteReport<ErrTrustAddConnectionResolved, string>
) => {
  switch (result.type) {
    case 'loading':
      return true;
    default:
      return false;
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
