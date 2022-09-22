import {
  DashboardState,
  Instant,
  Int,
  RemoteData,
  _Duration,
  _Instant,
  _Int,
  _RemoteData,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { useEffect, useState } from 'react';
import { toNativeRecord } from '../../../safe-as';

type ResultState = 'notAsked' | 'loading' | 'failed' | 'success';

type TrustAction = 'add' | 'remove';

type TrustResult = {
  action: TrustAction;
  lastState: ResultState;
  retry: number;
  timestamp: number;
};

export const useTrustResult = (
  address: string,
  trustAddResult: DashboardState['trustAddResult'],
  trustRemoveResult: DashboardState['trustRemoveResult']
): TrustResult | null => {
  const [trustResult, setTrustResult] = useState<TrustResult | null>(null);

  useEffect(() => {
    const addRes = getResultForAddress(address, trustAddResult);
    const remRes = getResultForAddress(address, trustRemoveResult);

    if (remRes && addRes) {
      const mostRecentRes =
        remRes.timestamp > addRes.timestamp ? remRes : addRes;
      setTrustResult(mostRecentRes);
    } else if (remRes) {
      setTrustResult(remRes);
    } else if (addRes) {
      setTrustResult(addRes);
    } else {
      setTrustResult(null);
    }
  }, [trustAddResult, trustRemoveResult]);

  return trustResult;
};

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const getResultForAddress = (
  address: string,
  results:
    | DashboardState['trustAddResult']
    | DashboardState['trustRemoveResult']
) => {
  const jsObj: Record<
    string,
    RemoteData<
      unknown,
      { retry: Int; timestamp: Instant },
      { retry: Int; timestamp: Instant },
      { retry: Int; timestamp: Instant }
    >
  > = toNativeRecord(results);

  if (address in jsObj) {
    const res = jsObj[address];
    return pipe(
      res,
      _RemoteData.unRemoteData({
        onNotAsked: () => null,
        onLoading: x =>
          mkTrustResult(
            'remove',
            'loading',
            _Int.toNumber(x.retry),
            _Duration.unMilliseconds(_Instant.unInstant(x.timestamp))
          ),
        onFailure: x =>
          mkTrustResult(
            'remove',
            'failed',
            _Int.toNumber(x.retry),
            _Duration.unMilliseconds(_Instant.unInstant(x.timestamp))
          ),
        onSuccess: x =>
          mkTrustResult(
            'remove',
            'success',
            _Int.toNumber(x.retry),
            _Duration.unMilliseconds(_Instant.unInstant(x.timestamp))
          ),
      })
    );
  }
  return null;
};

const mkTrustResult = (
  action: TrustAction,
  state: ResultState,
  retry: number,
  timestamp: number
): TrustResult => ({
  action,
  lastState: state,
  timestamp,
  retry,
});
