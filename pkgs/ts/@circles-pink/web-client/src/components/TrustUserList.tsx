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
};

export const TrustUserList = ({
  title,
  content,
  theme,
  icon,
  actionRow,
  setActiveOverlay,
  setOverlayOpen,
  setOverwriteTo,
  addTrust,
  removeTrust,
}: TrustUserListProps) => {
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
                <TableRow theme={theme} key={index}>
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
                        path={
                          c.isIncoming ? mdiAccountArrowLeft : mdiAccountCancel
                        }
                        size={1.6}
                        color={c.isIncoming ? theme.baseColor : 'white'}
                      />
                    </JustifyAroundCenter>
                  </TableData>
                  <TableData>
                    <JustifyAroundCenter>
                      <Icon
                        path={
                          c.isOutgoing ? mdiAccountArrowRight : mdiAccountCancel
                        }
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
                            if (
                              setActiveOverlay &&
                              setOverlayOpen &&
                              setOverwriteTo
                            ) {
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
                    </JustifyBetweenCenter>
                  </TableData>
                </TableRow>
              );
            })}
          </TableBody>
        </Table>
      </TableContainer>
    </Frame>
  );
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
