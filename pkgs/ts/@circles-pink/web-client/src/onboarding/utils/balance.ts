import { Balance } from '@circles-pink/state-machine/output/CirclesCore.Bindings';
// import { BN } from 'ethereumjs-util';

export const mapBalanceToBN = (raw: number): string => {
  const rawBalance = Math.round(raw * 100).toString();
  // Map and round balance to big number format
  return rawBalance + '0000000000000000';
};

export const mapBalanceToHr = (raw: Balance): string => {
  const rawBalance = parseInt(raw.toString());
  // Map and round balance to human readable format
  return (
    Math.floor(rawBalance / 1000 / 1000 / 1000 / 1000 / 1000 / 10) / 100
  ).toFixed(2);
};

// export const scaleBalanceToBN = (raw: number): string =>
//   new BN((raw * 1000000000000000000).toString());

// export const unscaleBalanceToHr = (raw: BN): string =>
//   raw.div(new BN('1000000000000000'));
