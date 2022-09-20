import React, { ReactElement, SetStateAction } from 'react';
import { SelectedOffer } from '.';
import { ConfirmSend } from './ConfirmSend';
import { Receive } from './Receive';
import { Send, SendProps } from './Send';

export type OverlayTag = 'SEND' | 'RECEIVE' | 'CONFIRM_SEND';

type DashboardOverlayProps = SendProps & {
  overlay: OverlayTag;
  selectedOffer?: SelectedOffer;
  setJustBoughtVoucher: React.Dispatch<SetStateAction<boolean>>;
  xbgeSafeAddress?: string;
};

export const DashboardOverlay = ({
  overlay,
  state,
  act,
  theme,
  setJustBoughtVoucher,
  overwriteTo,
  selectedOffer,
  xbgeSafeAddress,
}: DashboardOverlayProps): ReactElement | null => {
  switch (overlay) {
    case 'SEND':
      return (
        <Send overwriteTo={overwriteTo} state={state} act={act} theme={theme} />
      );
    case 'RECEIVE':
      return <Receive state={state} act={act} theme={theme} />;
    case 'CONFIRM_SEND':
      return selectedOffer ? (
        <ConfirmSend
          selectedOffer={selectedOffer}
          state={state}
          act={act}
          theme={theme}
          setJustBoughtVoucher={setJustBoughtVoucher}
          xbgeSafeAddress={xbgeSafeAddress}
        />
      ) : null;
  }
};
