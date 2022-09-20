import { DashboardState, _RemoteData } from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { useEffect, useState } from 'react';
import { getSumOfVouchers } from '../util';

export const useBoughtVouchersAmount = (
  vouchersResult: DashboardState['vouchersResult']
): number => {
  const [boughtVouchersAmount, setBoughtVouchersAmount] = useState(0);

  useEffect(() => {
    pipe(
      vouchersResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {},
        onFailure: () => {},
        onSuccess: x => {
          setBoughtVouchersAmount(getSumOfVouchers(x.data));
        },
      })
    );
  }, [vouchersResult]);

  return boughtVouchersAmount;
};
