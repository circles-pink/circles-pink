import { _RemoteData } from '@circles-pink/state-machine/src';
import { ButtonState } from '../../components/forms/Button';

export const mapResult = _RemoteData.unRemoteData<
  unknown,
  unknown,
  unknown,
  unknown,
  ButtonState
>({
  onNotAsked: () => 'enabled',
  onFailure: () => 'enabled',
  onSuccess: () => 'enabled',
  onLoading: () => 'loading',
});
