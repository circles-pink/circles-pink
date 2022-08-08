import { VoucherProvider } from '@circles-pink/state-machine/output/VoucherServer.Types';
import React, { ReactElement } from 'react';
import { Button } from '../../../components/forms';
import { Claim } from '../../../components/text';
import { Theme } from '../../../context/theme';

type BuyVouchersProps = {
  providers: Array<VoucherProvider>;
  theme: Theme;
};

export const BuyVouchers = ({
  providers,
  theme,
}: BuyVouchersProps): ReactElement => {
  return (
    <>
      <Claim color={theme.baseColor}>Buy Vouchers:</Claim>
      {providers.map(provider => {
        return (
          <div key={provider.id}>
            <h2>{provider.name}</h2>
            {provider.availableOffers.map(offer => {
              return (
                <div key={offer.amount}>
                  {offer.countAvailable}x: {offer.amount}€ <Button>BUY!</Button>
                </div>
              );
            })}
          </div>
        );
      })}
    </>
  );
};
