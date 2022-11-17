import { mdiCashFast } from '@mdi/js';
import React, {
  ReactElement,
  SetStateAction,
  useEffect,
  useState,
} from 'react';
import { Button } from '../../../components/forms';
import { JustifyEnd, Margin } from '../../../components/helper';
import { Claim, SubClaim } from '../../../components/text';
import { Theme } from '../../../context/theme';
import { mapResult } from '../../utils/mapResult';
import { convertTcToCrc } from '../../utils/timeCircles';
const Web3 = require('web3');
import { DashboardProps, SelectedOffer } from '../dashboard';
import { t } from 'i18next';
import { env } from '../../../env';
import BN from 'bn.js';
import {
  Address,
  VoucherProvider,
  _Address,
  _Nullable,
  _RemoteData,
  _StateMachine,
  _VoucherServer,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { fromNativeBN } from '../../../safe-as';

const { _dashboardAction } = _StateMachine;

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
  state,
  act,
  theme,
  selectedOffer: [provider, offer],
  setJustBoughtVoucher,
  xbgeSafeAddress,
}: ConfirmSendProps): ReactElement | null => {
  const optionAddr: Address | null = pipe(
    env.xbgeSafeAddress,
    _Address.parseAddress,
    _Nullable.toNullable
  );

  // State
  const [from, _] = useState<Address>(_Address.Address(state.user.safeAddress));
  const [to, setTo] = useState<Address>();
  const [value, setValue] = useState<BN>(
    eurToCrc(_VoucherServer.unVoucherAmount(offer.amount))
  );

  useEffect(() => {
    if (xbgeSafeAddress) {
      const optionAddrProp: Address | null = pipe(
        xbgeSafeAddress,
        _Address.parseAddress,
        _Nullable.toNullable
      );

      if (optionAddrProp) {
        setTo(optionAddrProp);
      } else if (optionAddr) {
        setTo(optionAddr);
      }
    } else if (optionAddr) {
      setTo(optionAddr);
    }
  }, [env]);

  useEffect(() => {
    setValue(eurToCrc(_VoucherServer.unVoucherAmount(offer.amount)));
  }, [provider, offer]);

  useEffect(() => {
    const result = pipe(
      state.transferResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => null,
        onLoading: () => null,
        onFailure: () => null,
        onSuccess: x => x,
      })
    );

    if (result) {
      // console.log('Show waiting voucher');
      setJustBoughtVoucher(true);
    }
  }, [state.transferResult]);

  // Util
  const transact = (fromAddr: Address, toAddr: Address) =>
    act(
      _dashboardAction._transfer({
        from: fromAddr,
        to: toAddr,
        value: fromNativeBN(value),
        paymentNote: _VoucherServer.unVoucherProviderId(provider.id),
      })
    );

  // Render

  const ConfirmContent = () => (
    <>
      <Claim color={theme.baseColor}>
        {t('dashboard.voucherShop.confirmSendClaim')}
      </Claim>
      <ConfirmDialog
        theme={theme}
        provider={provider}
        eurAmount={_VoucherServer.unVoucherAmount(offer.amount)}
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
        </Button>
      </JustifyEnd>
    </>
  );

  return pipe(
    state.transferResult,
    _RemoteData.unRemoteData({
      onNotAsked: () => <ConfirmContent />,
      onLoading: () => <ConfirmContent />,
      onFailure: () => (
        <Margin top={2} bottom={2}>
          <Claim color={theme.baseColor}>
            {t('dashboard.voucherShop.confirmSendProblem')}
          </Claim>
        </Margin>
      ),
      onSuccess: () => <ConfirmContent />,
    })
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
