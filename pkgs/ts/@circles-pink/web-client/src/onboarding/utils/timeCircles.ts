const Web3 = require('web3');
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

export const convertTcToCrc = (amount: number, date: TcDate = new Date()) => {
  return mapCircles(amount, date, 'FROM-TIME-CIRCLES');
};

export const convertCrcToTc = (amount: number, date: TcDate = new Date()) => {
  return mapCircles(amount, date, 'TO-TIME-CIRCLES');
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
  date: TcDate = new Date()
) {
  return mapCurrency(amount, date, currency).toFixed(2);
}

const mapCurrency = (amount: string, date: TcDate, type: Currency) => {
  const freckles = Number.parseFloat(Web3.utils.fromWei(amount, 'ether'));

  switch (type) {
    case 'CIRCLES':
      return freckles;
    case 'TIME-CIRCLES':
      return convertCrcToTc(freckles, date);
    case 'EURO':
      return convertCrcToTc(freckles, date) / 10;
  }
};
