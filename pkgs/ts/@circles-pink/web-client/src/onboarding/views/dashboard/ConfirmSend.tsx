import { mdiCashFast } from '@mdi/js';
import * as A from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.Action';
import React, { useEffect, useState } from 'react';
import { Button } from '../../../components/forms';
import { JustifyEnd } from '../../../components/helper';
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
import styled from '@emotion/styled';
import tw, { theme } from 'twin.macro';
import { Address } from '@circles-pink/state-machine/output/CirclesPink.Data.Address';
import { Option } from 'fp-ts/lib/Option';
import { toFpTsOption } from '../../../utils/fpTs';
import { env } from '../../../env';
import BN from 'bn.js';
import { VoucherProvider } from '@circles-pink/state-machine/output/VoucherServer.Types';

// -----------------------------------------------------------------------------
// ConfirmSend Circles
// -----------------------------------------------------------------------------

export type ConfirmSendProps = DashboardProps & {
  theme: Theme;
  selectedOffer: SelectedOffer;
};

export const ConfirmSend = ({
  state: stateRaw,
  act,
  theme,
  selectedOffer: [provider, offer],
}: ConfirmSendProps) => {
  const state = (defaultView as any)(stateRaw) as DefaultView;

  // State
  const [from, _] = useState<Address>(stateRaw.user.safeAddress);
  const [to, setTo] = useState<Address>();
  const [value, setValue] = useState<BN>(eurToCrc(offer.amount));

  useEffect(() => {
    const optionAddr: Option<Address> = toFpTsOption(
      parseAddress(env.xbgeSafeAddress)
    );
    if (optionAddr._tag === 'Some') {
      setTo(optionAddr.value);
    }
  }, [env]);

  useEffect(() => {
    setValue(eurToCrc(offer.amount));
  }, [provider, offer]);

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

  if (!to || state.transferResult.type === 'failure') {
    return (
      <Claim color={theme.baseColor}>
        {t('dashboard.voucherShop.confirmSendProblem')}
      </Claim>
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
          state={mapResult(state.transferResult)}
          icon={mdiCashFast}
          onClick={() => transact(from, to)}
        >
          {t('dashboard.voucherShop.confirmSendButton')}
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
