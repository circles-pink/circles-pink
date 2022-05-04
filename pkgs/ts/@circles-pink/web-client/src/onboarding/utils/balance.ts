import { Balance } from 'generated/output/CirclesCore.Bindings';

export const mapBalanceToBN = (raw: number): string => {
  const rawBalance = (raw * 100).toString();
  // Map and round balance to big number format
  // Todo: Replace! Ugly but works for the moment...
  return rawBalance + '0000000000000000';
};

export const mapBalanceToHr = (raw: Balance): string => {
  const rawBalance = parseInt(raw.toString());
  // Map and round balance to human readable format
  return (
    Math.floor(rawBalance / 1000 / 1000 / 1000 / 1000 / 1000 / 10) / 100
  ).toFixed(2);
};
