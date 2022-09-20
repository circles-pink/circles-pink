import { Voucher, _VoucherServer } from '@circles-pink/state-machine/src';

export const getSumOfVouchers = (vouchers: ReadonlyArray<Voucher>) =>
  vouchers.reduce((p, c) => p + _VoucherServer.unVoucherAmount(c.amount), 0);
