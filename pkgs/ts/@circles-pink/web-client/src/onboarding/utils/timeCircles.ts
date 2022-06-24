import Web3 from 'web3';
import { crcToTc, tcToCrc } from '@circles/timecircles';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type Currency = 'CIRCLES' | 'TIME-CIRCLES' | 'EURO';

type Conversion = 'FROM-TIME-CIRCLES' | 'TO-TIME-CIRCLES';

type TcDate = Date | number;

// -----------------------------------------------------------------------------
// Circles / Timecircles conversion
// -----------------------------------------------------------------------------

export const convertTimeCirclesToCircles = (amount: number, date?: TcDate) => {
  const dateTime = date || new Date();
  return mapCircles(amount, dateTime, 'FROM-TIME-CIRCLES');
};

export const convertCirclesToTimeCircles = (amount: number, date?: TcDate) => {
  const dateTime = date || new Date();
  return mapCircles(amount, dateTime, 'TO-TIME-CIRCLES');
};

const mapCircles = (amount: number, date: TcDate, type: Conversion) => {
  switch (type) {
    case 'FROM-TIME-CIRCLES':
      return tcToCrc(date, amount);
    case 'TO-TIME-CIRCLES':
      return crcToTc(date, amount);
  }
};

// -----------------------------------------------------------------------------
// Balance
// -----------------------------------------------------------------------------

export function displayBalance(
  amount: string,
  currency: Currency = 'TIME-CIRCLES',
  date?: TcDate
) {
  const dateTime = date || new Date();
  return mapCurrency(amount, dateTime, currency).toFixed(2);
}

const mapCurrency = (amount: string, dateTime: TcDate, type: Currency) => {
  switch (type) {
    case 'CIRCLES':
      return Number.parseFloat(Web3.utils.fromWei(amount, 'ether'));
    case 'TIME-CIRCLES':
      return convertCirclesToTimeCircles(
        Number.parseFloat(Web3.utils.fromWei(amount, 'ether')),
        dateTime
      );
    case 'EURO':
      return (
        convertCirclesToTimeCircles(
          Number.parseFloat(Web3.utils.fromWei(amount, 'ether')),
          dateTime
        ) / 10
      );
  }
};
