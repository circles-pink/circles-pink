import { RemoteData } from '@circles-pink/state-machine/output/RemoteData';
import { _RemoteData } from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';

export const mapIndicatorColors = (
  apiResult: RemoteData<unknown, unknown, unknown, { isValid: boolean }>
) =>
  pipe(
    apiResult,
    _RemoteData.unRemoteData({
      onFailure: () => 'red',
      onLoading: () => 'black',
      onSuccess: ({ isValid }) => isValid ? 'green' : 'red',
      onNotAsked: () => 'black',
    })
  );

