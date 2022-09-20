import {
  DashboardAction,
  DashboardState,
  unit,
  _RemoteData,
  _StateMachine,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { useEffect } from 'react';

const { _dashboardAction } = _StateMachine;

export const useHealthCheck = (
  act: (ac: DashboardAction) => void,
  checkUBIPayoutResult: DashboardState['checkUBIPayoutResult']
) => {
  useEffect(() => {
    pipe(
      checkUBIPayoutResult,
      _RemoteData.unRemoteData({
        onNotAsked: () => {},
        onLoading: () => {},
        onFailure: x => {
          if (
            x.error.type === 'errNative' &&
            x.error.value.name === 'CoreError' &&
            x.error.value.message.includes('Invalid Token address')
          ) {
            act(_dashboardAction._redeploySafeAndToken(unit));
          }
        },
        onSuccess: () => {},
      })
    );
  }, [checkUBIPayoutResult]);
};
