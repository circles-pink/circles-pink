import { RemoteData } from '@circles-pink/state-machine/output/RemoteData';
import { ButtonState } from '../../components/forms/Button';

export const mapResult = <L, N, E, A>(
  remoteData: RemoteData<N, L, E, A>
): ButtonState => {
  switch (remoteData.type) {
    case 'failure':
      return 'enabled';
    case 'success':
      return 'enabled';
    case 'loading':
      return 'loading';
    case 'notAsked':
      return 'enabled';
  }
};
