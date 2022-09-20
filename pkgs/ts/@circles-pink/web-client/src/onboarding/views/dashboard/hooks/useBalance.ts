import { DashboardState, _RemoteData } from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { useEffect, useState } from 'react';
import { toNativeBN } from '../../../../safe-as';
import { displayBalance } from '../../../utils/timeCircles';

export const useBalance = (
  getBalanceResult: DashboardState['getBalanceResult']
): number => {
  const [userBalance, setUserBalance] = useState<number>(0);

  useEffect(() => {
    pipe(
      getBalanceResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {},
        onFailure: () => {},
        onSuccess: x => {
          setUserBalance(
            parseFloat(displayBalance(toNativeBN(x.data), 'TIME-CIRCLES'))
          );
        },
      })
    );
  }, [getBalanceResult]);

  return userBalance;
};
