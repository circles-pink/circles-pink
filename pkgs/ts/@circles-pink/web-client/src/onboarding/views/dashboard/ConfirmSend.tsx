import { mdiCashFast } from '@mdi/js';
import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import React, { SetStateAction, useEffect, useState } from 'react';
import { Button } from '../../../components/forms';
import { JustifyEnd, Margin } from '../../../components/helper';
import { Claim, SubClaim } from '../../../components/text';
import { Theme } from '../../../context/theme';
import { mapResult } from '../../utils/mapResult';
import { convertTcToCrc } from '../../utils/timeCircles';
const Web3 = require('web3');
import { DashboardProps, SelectedOffer } from '../dashboard';
import { t } from 'i18next';
import {
  DefaultView,
  defaultView,
  parseAddress,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State.Dashboard.Views';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { Option } from 'fp-ts/lib/Option';
import { toFpTsOption } from '../../../utils/fpTs';
import { env } from '../../../env';
import BN from 'bn.js';
import { VoucherProvider } from '@circles-pink/state-machine/output/VoucherServer.Spec.Types';

// -----------------------------------------------------------------------------
// ConfirmSend Circles
// -----------------------------------------------------------------------------

export type ConfirmSendProps = Omit<DashboardProps, 'buyVoucherEurLimit'> & {
  theme: Theme;
  selectedOffer: SelectedOffer;
  setJustBoughtVoucher: React.Dispatch<SetStateAction<boolean>>;
  xbgeSafeAddress?: string;
};

export const ConfirmSend = ({
  state: stateRaw,
  act,
  theme,
  selectedOffer: [provider, offer],
  setJustBoughtVoucher,
  xbgeSafeAddress,
}: ConfirmSendProps) => {
  const state = (defaultView as any)(stateRaw) as DefaultView;

  const optionAddr: Option<Address> = toFpTsOption(
    parseAddress(env.xbgeSafeAddress)
  );

  // State
  const [from, _] = useState<Address>(stateRaw.user.safeAddress);
  const [to, setTo] = useState<Address>();
  const [value, setValue] = useState<BN>(eurToCrc(offer.amount));

  useEffect(() => {
    if (xbgeSafeAddress) {
      const optionAddrProp: Option<Address> = toFpTsOption(
        parseAddress(xbgeSafeAddress)
      );
      if (optionAddrProp._tag === 'Some') {
        setTo(optionAddrProp.value);
      } else if (optionAddr._tag === 'Some') {
        setTo(optionAddr.value);
      }
    } else if (optionAddr._tag === 'Some') {
      setTo(optionAddr.value);
    }
  }, [env]);

  useEffect(() => {
    setValue(eurToCrc(offer.amount));
  }, [provider, offer]);

  useEffect(() => {
    if (state.transferResult.type === 'success') {
      // console.log('Show waiting voucher');
      setJustBoughtVoucher(true);
    }
  }, [state.transferResult]);

  // Util
  const transact = (fromAddr: Address, toAddr: Address) =>
    act(
      A._dashboard(
        A._transfer({
          from: fromAddr,
          to: toAddr,
          value: value as any,
          paymentNote: provider.id,
        })
      )
    );

  // Render

  if (state.transferResult.type === 'failure') {
    return (
      <Margin top={2} bottom={2}>
        <Claim color={theme.baseColor}>
          {t('dashboard.voucherShop.confirmSendProblem')}
        </Claim>
      </Margin>
    );
  }

  return (
    <>
      <Claim color={theme.baseColor}>
        {t('dashboard.voucherShop.confirmSendClaim')}
      </Claim>
      <ConfirmDialog
        theme={theme}
        provider={provider}
        eurAmount={offer.amount}
      />
      <JustifyEnd>
        <Button
          prio={'high'}
          theme={theme}
          icon={mdiCashFast}
          state={to ? mapResult(state.transferResult) : 'disabled'}
          onClick={() => (to ? transact(from, to) : {})}
        >
          {t('dashboard.voucherShop.confirmSendButton')}
          {to}
        </Button>
      </JustifyEnd>
    </>
  );
};

// -----------------------------------------------------------------------------
// ConfirmDialog
// -----------------------------------------------------------------------------

type ConfirmDialogProps = {
  theme: Theme;
  provider: VoucherProvider;
  eurAmount: number;
};

const ConfirmDialog = ({ theme, provider, eurAmount }: ConfirmDialogProps) => {
  const crcAmount = eurAmount * 10;

  return (
    <>
      <SubClaim color={theme.baseColor}>
        {`${t('dashboard.voucherShop.buyFrom')} ${provider.name}`}
        <br />
        {`${t('dashboard.voucherShop.tcCostWillBe')} ${crcAmount} CRC`}
        <br />
        {`${t('dashboard.voucherShop.eurAmountWillBe')} ${eurAmount} €`}
      </SubClaim>

      <Margin top={1} bottom={1}>
        <SubClaim color={theme.baseColor}>{t('mayTakeSomeTime')}</SubClaim>
      </Margin>
    </>
  );
};

// -----------------------------------------------------------------------------
// Euro to Circles Amount
// -----------------------------------------------------------------------------

const eurToCrc = (eur: number): BN => {
  const timeCircles = eur * 10;
  const circles = convertTcToCrc(timeCircles).toString();
  const freckles = Web3.utils.toWei(circles, 'ether');
  return new Web3.utils.BN(freckles);
};
