import {
  EmailApiResult,
  UsernameApiResult,
} from '@circles-pink/state-machine/output/CirclesPink.Garden.StateMachine.State';

export const mapIndicatorColors = (
  apiResult: UsernameApiResult | EmailApiResult
) => {
  switch (apiResult.type) {
    case 'notAsked':
    case 'loading':
      return 'black';
    case 'failure':
      return 'red';
    case 'success':
      return apiResult.value.isValid ? 'green' : 'red';
    default:
      return 'black';
  }
};
