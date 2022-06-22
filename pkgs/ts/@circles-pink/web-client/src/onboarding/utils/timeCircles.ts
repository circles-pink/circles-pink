import Web3 from 'web3';
import dayjs from 'dayjs';
import { crcToTc, tcToCrc } from '@circles/timecircles';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type Currency = 'CIRCLES' | 'TIME-CIRCLES' | 'EURO';

type Conversion = 'FROM-TIME-CIRCLES' | 'TO-TIME-CIRCLES';

// -----------------------------------------------------------------------------
// Circles / Timecircles conversion
// -----------------------------------------------------------------------------

export const convertTimeCirclesToCircles = (amount: number, date?: string) => {
  const dateTime = date ? dayjs(date) : dayjs();
  return mapCircles(amount, dateTime, 'FROM-TIME-CIRCLES');
};

export const convertCirclesToTimeCircles = (amount: number, date?: string) => {
  const dateTime = date ? dayjs(date) : dayjs();
  return mapCircles(amount, dateTime, 'TO-TIME-CIRCLES');
};

const mapCircles = (
  amount: number,
  dateTime: dayjs.Dayjs,
  type: Conversion
) => {
  const transactionDateUnix = dayjs(dateTime).unix();

  switch (type) {
    case 'FROM-TIME-CIRCLES':
      return tcToCrc(transactionDateUnix, amount);
    case 'TO-TIME-CIRCLES':
      return crcToTc(transactionDateUnix, amount);
  }
};

// -----------------------------------------------------------------------------
// Balance
// -----------------------------------------------------------------------------

export function displayBalance(
  amount: string,
  currency: Currency = 'TIME-CIRCLES',
  date?: string
) {
  const dateTime = date ? dayjs(date) : dayjs();
  return mapCurrency(amount, dateTime, currency).toFixed(2);
}

const mapCurrency = (amount: string, dateTime: dayjs.Dayjs, type: Currency) => {
  switch (type) {
    case 'CIRCLES':
      return Number.parseFloat(Web3.utils.fromWei(amount, 'ether'));
    case 'TIME-CIRCLES':
      return convertCirclesToTimeCircles(
        Number.parseFloat(Web3.utils.fromWei(amount, 'ether')),
        dateTime.toString()
      );
    case 'EURO':
      return (
        convertCirclesToTimeCircles(
          Number.parseFloat(Web3.utils.fromWei(amount, 'ether')),
          dateTime.toString()
        ) / 10
      );
  }
};
