import { Unit } from 'generated/output/Data.Unit';

type ApiResult =
  | {
      type: 'notAsked';
      value: Unit;
    }
  | {
      type: 'loading';
      value: Unit;
    }
  | {
      type: 'failure';
      value:
        | {
            type: 'errNetwork';
            value: Unit;
          }
        | {
            type: 'errService';
            value: Unit;
          }
        | {
            type: 'errParse';
            value: Unit;
          };
    }
  | {
      type: 'success';
      value: {
        isValid: boolean;
      };
    };

export const mapIndicatorColors = (apiResult: ApiResult) => {
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
