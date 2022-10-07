import {
  DashboardAction,
  unit,
  _StateMachine,
} from '@circles-pink/state-machine/src';
import { useEffect } from 'react';

const { _dashboardAction } = _StateMachine;

const BALANCE_INTERVAL = 10000;
const TRUST_NETWORK_INTERVAL = 15000;
const UBI_PAYOUT_INTERVAL = 60000;
const VOUCHER_INTERVAL = 30000;

export const useClientPolling = (act: (ac: DashboardAction) => void) => {
  useEffect(() => {
    // Gather initial client information
    act(_dashboardAction._getBalance(unit));
    act(_dashboardAction._getTrusts(unit));
    act(_dashboardAction._getUBIPayout(unit));
    act(_dashboardAction._getVouchers(getTimestamp()));
    act(_dashboardAction._getVoucherProviders(unit));

    // Start polling tasks
    const balancePolling = setInterval(() => {
      act(_dashboardAction._getBalance(unit));
    }, BALANCE_INTERVAL);

    // const trustNetworkPolling = setInterval(() => {
    //   act(_dashboardAction._getTrusts(unit));
    // }, TRUST_NETWORK_INTERVAL);

    const ubiPayoutPolling = setInterval(() => {
      act(_dashboardAction._getUBIPayout(unit));
    }, UBI_PAYOUT_INTERVAL);

    const voucherPolling = setInterval(() => {
      act(_dashboardAction._getVouchers(getTimestamp()));
    }, VOUCHER_INTERVAL);

    const voucherProviderPolling = setInterval(() => {
      act(_dashboardAction._getVoucherProviders(unit));
    }, VOUCHER_INTERVAL);

    // Clear interval on unmount
    return () => {
      clearInterval(balancePolling);
      // clearInterval(trustNetworkPolling);
      clearInterval(ubiPayoutPolling);
      clearInterval(voucherPolling);
      clearInterval(voucherProviderPolling);
    };
  }, []);
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const getTimestamp = () => Math.round(new Date().getTime() / 1000).toString();
