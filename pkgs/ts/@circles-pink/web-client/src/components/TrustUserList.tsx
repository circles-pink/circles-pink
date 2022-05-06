import React, { ReactElement, SetStateAction } from 'react';
import tw, { css, styled } from 'twin.macro';
import { TrustNode } from '@circles-pink/state-machine/output/CirclesCore';
import { Theme } from '../context/theme';
import { Claim } from './text';
import {
  mdiAccountArrowLeft,
  mdiAccountArrowRight,
  mdiAccountCancel,
  mdiHeart,
  mdiHeartOutline,
  mdiCashFast,
  mdiCashRemove,
  mdiAt,
} from '@mdi/js';
import Icon from '@mdi/react';
import { darken } from '../onboarding/utils/colorUtils';
import { addrToString } from '@circles-pink/state-machine/output/Wallet.PrivateKey';
import {
  JustifyAroundCenter,
  JustifyBetweenCenter,
  JustifyStartCenter,
} from './helper';
import {
  ErrTrustAddConnectionResolved,
  TrustAddResult,
  TrustRemoveResult,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';
import { RemoteReport } from '@circles-pink/state-machine/output/RemoteReport';
import { LoadingCircles } from './LoadingCircles';

type UserData = {
  username: string;
  avatarUrl: string | null;
};

export type MappedTrustNodes = Array<TrustNode & UserData>;

type Overlay = 'SEND' | 'RECEIVE';

type TrustUserListProps = {
  title?: string;
  content: MappedTrustNodes;
  theme: Theme;
  icon: any;
  actionRow?: ReactElement | ReactElement[] | string;
  setActiveOverlay?: React.Dispatch<SetStateAction<Overlay>>;
  setOverlayOpen?: React.Dispatch<SetStateAction<boolean>>;
  setOverwriteTo?: React.Dispatch<SetStateAction<string>>;
  addTrust: (to: string) => void;
  removeTrust: (to: string) => void;
  trustAddResult: TrustAddResult;
  trustRemoveResult: TrustRemoveResult;
};

export const TrustUserList = (props: TrustUserListProps) => {
  const { title, content, theme, icon, actionRow } = props;

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
          {content.length > 0 && (
            <TableHeader>
              <TableRow theme={theme}>
                <TableHead>User</TableHead>
                {/* <TableHead>Safe Address</TableHead> */}
                <TableHead>
                  <JustifyAround>Receivable</JustifyAround>
                </TableHead>
                <TableHead>
                  <JustifyAround>Sendable</JustifyAround>
                </TableHead>
                {/* <TableHead>
                <JustifyAround>Transferable</JustifyAround>
              </TableHead> */}
                <TableHead>
                  <JustifyAround>Action</JustifyAround>
                </TableHead>
              </TableRow>
            </TableHeader>
          )}

          <TableBody>
            {content.map((c, index) => {
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

const ContentRow = (
  props: TrustUserListProps & { c: TrustNode & UserData }
): ReactElement => {
  const {
    c,
    theme,
    setActiveOverlay,
    setOverlayOpen,
    setOverwriteTo,
    addTrust,
    removeTrust,
    trustAddResult,
    trustRemoveResult,
  } = props;

  const trustAddLoading = trustAddResult[addrToString(c.safeAddress)]
    ? trustIsLoading(trustAddResult[addrToString(c.safeAddress)])
    : false;

  const trustRemoveLoading = trustRemoveResult[addrToString(c.safeAddress)]
    ? trustIsLoading(trustRemoveResult[addrToString(c.safeAddress)])
    : false;

  return (
    <TableRow theme={theme}>
      <TableData>
        <JustifyStartCenter>
          <Icon path={mdiAt} size={1.5} color={theme.baseColor} />
          <b>{c.username}</b>
        </JustifyStartCenter>
      </TableData>
      {/* <TableData>{c.safeAddress}</TableData> */}
      <TableData>
        <JustifyAroundCenter>
          <Icon
            path={c.isIncoming ? mdiAccountArrowLeft : mdiAccountCancel}
            size={1.6}
            color={c.isIncoming ? theme.baseColor : 'white'}
          />
        </JustifyAroundCenter>
      </TableData>
      <TableData>
        <JustifyAroundCenter>
          <Icon
            path={c.isOutgoing ? mdiAccountArrowRight : mdiAccountCancel}
            size={1.6}
            color={c.isOutgoing ? theme.baseColor : 'white'}
          />
        </JustifyAroundCenter>
      </TableData>
      {/* <TableData>
            <JustifyAround>y €</JustifyAround>
          </TableData> */}
      <TableData>
        <JustifyBetweenCenter>
          <Clickable
            clickable={c.isOutgoing}
            onClick={() => {
              if (c.isOutgoing) {
                if (setActiveOverlay && setOverlayOpen && setOverwriteTo) {
                  setOverwriteTo(addrToString(c.safeAddress));
                  setActiveOverlay('SEND');
                  setOverlayOpen(true);
                }
              }
            }}
          >
            <Icon
              path={c.isOutgoing ? mdiCashFast : mdiCashRemove}
              size={1.75}
              color={c.isOutgoing ? theme.baseColor : 'white'}
            />
          </Clickable>

          {!trustAddLoading && !trustRemoveLoading ? (
            <Clickable
              clickable={true}
              onClick={() => {
                if (!c.isIncoming) {
                  addTrust(addrToString(c.safeAddress));
                } else {
                  removeTrust(addrToString(c.safeAddress));
                }
              }}
            >
              <Icon
                path={c.isIncoming ? mdiHeart : mdiHeartOutline}
                size={1.5}
                color={c.isIncoming ? theme.baseColor : 'white'}
              />
            </Clickable>
          ) : (
            <LoadingCircles count={1} width={35} color={theme.baseColor} />
          )}
        </JustifyBetweenCenter>
      </TableData>
    </TableRow>
  );
};

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
// UI / Frame
// -----------------------------------------------------------------------------

type FameProps = {
  theme: Theme;
};

const Frame = styled.div<FameProps>(({ theme }: FameProps) => [
  tw`block p-8 border border-gray-800 shadow-xl rounded-xl`,
  css`
    background-color: ${theme.lightColor};
  `,
]);

// -----------------------------------------------------------------------------
// UI / Table
// -----------------------------------------------------------------------------

const TableContainer = tw.div`overflow-hidden overflow-x-auto border border-gray-100 rounded`;
const Table = tw.table`min-w-full text-sm divide-y divide-gray-200`;
const TableHeader = tw.thead`px-4 py-2 lg:text-lg text-left whitespace-nowrap`;
const TableHead = tw.th`px-4 py-2 text-left whitespace-nowrap`;
const TableBody = tw.tbody`divide-y divide-gray-100`;
const TableData = tw.td`px-4 py-2 text-lg whitespace-nowrap`;

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
