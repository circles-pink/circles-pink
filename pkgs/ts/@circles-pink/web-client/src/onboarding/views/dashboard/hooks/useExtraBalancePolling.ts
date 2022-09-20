import {
  DashboardAction,
  DashboardState,
  unit,
  _RemoteData,
  _StateMachine,
  _VoucherServer,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { useEffect, useState } from 'react';
import { getSumOfVouchers } from '../util';
import { useBoughtVouchersAmount } from './useBoughtVouchersAmount';

const { _dashboardAction } = _StateMachine;

const MAX_RETRYS = 10;
const RETRY_INTERVAL = 1000;

// -----------------------------------------------------------------------------
// Poll Balance after a voucher was bought
// -----------------------------------------------------------------------------

export const useVoucherBalancePolling = (
  act: (ac: DashboardAction) => void,
  vouchersResult: DashboardState['vouchersResult'],
  transferResult: DashboardState['transferResult']
) => {
  const [initAmount, setinitAmount] = useState<string | null>(null);
  const [countRefreshTransfer, setCountRefreshTransfer] = useState<number>(0);
  const boughtVouchersAmount =
    useBoughtVouchersAmount(vouchersResult).toString();

  useEffect(() => {
    pipe(
      transferResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {
          setCountRefreshTransfer(0);
          setinitAmount(boughtVouchersAmount);
        },
        onFailure: () => {},
        onSuccess: () => {
          if (
            boughtVouchersAmount === initAmount &&
            countRefreshTransfer < MAX_RETRYS
          ) {
            setTimeout(() => {
              act(_dashboardAction._getBalance(unit));
              setCountRefreshTransfer(countRefreshTransfer + 1);
            }, RETRY_INTERVAL);
          }
        },
      })
    );
  }, [transferResult, countRefreshTransfer]);
};

// -----------------------------------------------------------------------------
// Poll Balance after UBI Payout was requested
// -----------------------------------------------------------------------------

export const useUBIPayoutBalancePolling = (
  act: (ac: DashboardAction) => void,
  getBalanceResult: DashboardState['getBalanceResult'],
  requestUBIPayoutResult: DashboardState['requestUBIPayoutResult']
) => {
  const [initBalance, setInitBalance] = useState<string | null>(null);
  const [countRefreshPayout, setCountRefreshPayout] = useState<number>(0);

  useEffect(() => {
    const balance = pipe(
      getBalanceResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => null,
        onLoading: () => null,
        onFailure: () => null,
        onSuccess: x => x.data.toString(),
      })
    );

    pipe(
      requestUBIPayoutResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {
          setCountRefreshPayout(0);
          setInitBalance(balance);
        },
        onFailure: () => {},
        onSuccess: () => {
          if (balance === initBalance && countRefreshPayout < MAX_RETRYS) {
            setTimeout(() => {
              act(_dashboardAction._getBalance(unit));
              setCountRefreshPayout(countRefreshPayout + 1);
            }, RETRY_INTERVAL);
          }
        },
      })
    );
  }, [requestUBIPayoutResult, countRefreshPayout]);
};
