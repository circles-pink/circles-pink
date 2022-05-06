import Web3 from 'web3';
import dayjs from 'dayjs';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type Currency = 'CIRCLES' | 'TIME-CIRCLES' | 'EURO';

type Conversion = 'FROM-TIME-CIRCLES' | 'TO-TIME-CIRCLES';

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

const oneYearInSeconds = 31557600; // This is 365,25 Days in seconds.
const oneDayInSeconds = 86400;
const day0Unix = dayjs('2020-10-15T00:00:00.000Z').unix();

const baseCirclesPerDayValue = 8;
let previousCirclesPerDayValue = 8;

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const circlesValue = (x: number) => x * 1.07;
const lerp = (x: number, y: number, a: number) => x * (1 - a) + y * a;

function getBaseCirclesPerDayValue(yearsSince: number) {
  let circlesPerDayValue = baseCirclesPerDayValue;
  for (let index = 0; index < yearsSince; index++) {
    previousCirclesPerDayValue = circlesPerDayValue;
    circlesPerDayValue = circlesValue(circlesPerDayValue);
  }
  return circlesPerDayValue;
}

// -----------------------------------------------------------------------------
// Circles / Timecircles conversion
// -----------------------------------------------------------------------------

export function convertTimeCirclesToCircles(amount: number, date?: string) {
  const dateTime = date ? dayjs(date) : dayjs();
  return mapCircles(amount, dateTime, 'FROM-TIME-CIRCLES');
}

export function convertCirclesToTimeCircles(amount: number, date?: string) {
  const dateTime = date ? dayjs(date) : dayjs();
  return mapCircles(amount, dateTime, 'TO-TIME-CIRCLES');
}

const mapCircles = (
  amount: number,
  dateTime: dayjs.Dayjs,
  type: Conversion
) => {
  const transactionDateUnix = dayjs(dateTime).unix();
  const daysSinceDay0Unix = (transactionDateUnix - day0Unix) / oneDayInSeconds;
  const dayInCurrentCycle = daysSinceDay0Unix % 365.25;
  const yearsSince = (transactionDateUnix - day0Unix) / oneYearInSeconds;
  const perDayValue = getBaseCirclesPerDayValue(yearsSince);

  switch (type) {
    case 'FROM-TIME-CIRCLES':
      return parseFloat(
        (
          (amount / 24) *
          lerp(
            previousCirclesPerDayValue,
            perDayValue,
            dayInCurrentCycle / 365.25
          )
        ).toFixed(12)
      );
    case 'TO-TIME-CIRCLES':
      return (
        (amount /
          lerp(
            previousCirclesPerDayValue,
            perDayValue,
            dayInCurrentCycle / 365.25
          )) *
        24
      );
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
