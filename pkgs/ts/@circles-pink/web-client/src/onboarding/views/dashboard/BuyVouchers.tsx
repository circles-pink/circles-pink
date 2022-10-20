import {
  VoucherProvider,
  VoucherProvidersResult,
  _RemoteData,
  _RemoteReport,
  _VoucherServer,
} from '@circles-pink/state-machine/src';
import { pipe } from 'fp-ts/lib/function';
import { t } from 'i18next';
import React, { ReactElement, useEffect, useState } from 'react';
import { css, styled } from 'twin.macro';
import { SelectedOffer } from '.';
import { Margin } from '../../../components/helper';
import { Claim, JustText, SubClaim } from '../../../components/text';
import { Theme } from '../../../context/theme';

type BuyVouchersProps = {
  providers: VoucherProvidersResult;
  theme: Theme;
  initializeVoucherOrder: (selectedOffer: SelectedOffer) => void;
  availableBalance: number;
  boughtVouchersAmount: number;
  buyVoucherEurLimit: number;
};

export const BuyVouchers = ({
  providers,
  theme,
  initializeVoucherOrder,
  availableBalance,
  boughtVouchersAmount,
  buyVoucherEurLimit,
}: BuyVouchersProps): ReactElement => {
  const [providers_, setProviders_] = useState(
    pipe(providers, _RemoteReport.getData<ReadonlyArray<VoucherProvider>>([]))
  );

  useEffect(() => {
    const providerResult = pipe(
      providers,
      _RemoteReport.getData<ReadonlyArray<VoucherProvider>>([])
    );
    if (providerResult.length > 0) {
      setProviders_(providerResult);
    }
  }, [providers]);

  const limitReached = boughtVouchersAmount >= buyVoucherEurLimit;

  const canBuyOffer = (num: number): boolean => {
    if (boughtVouchersAmount + num <= buyVoucherEurLimit) {
      return true;
    }
    return false;
  };

  return (
    <>
      <Claim color={theme.textColorDark}>
        {t('dashboard.voucherShop.buyTitle')}
      </Claim>
      <Margin top={1} bottom={1}>
        {!limitReached ? (
          <JustText>
            {t('dashboard.voucherShop.buyDescription')
              .replace(
                '{{limit}}',
                `${buyVoucherEurLimit}€ (${buyVoucherEurLimit * 10} Circles)`
              )
              .replace('{{amount}}', boughtVouchersAmount.toString())}
          </JustText>
        ) : (
          <JustText>
            {t('dashboard.voucherShop.buyLimitReached').replace(
              '{{limit}}',
              `${buyVoucherEurLimit}€ (${buyVoucherEurLimit * 10} Circles)`
            )}
          </JustText>
        )}
      </Margin>
      {providers_.length > 0 ? (
        <>
          {providers_.map(provider => {
            const sortedOffers = [...provider.availableOffers].sort(
              (a, b) =>
                _VoucherServer.unVoucherAmount(a.amount) -
                _VoucherServer.unVoucherAmount(b.amount)
            );

            return (
              <div key={_VoucherServer.unVoucherProviderId(provider.id)}>
                <OfferContainer elementCount={provider.availableOffers.length}>
                  {sortedOffers.map(offer => {
                    const userCanBuy =
                      availableBalance -
                        _VoucherServer.unVoucherAmount(offer.amount) * 10 >
                      0;

                    const offerInLimits =
                      !limitReached &&
                      canBuyOffer(_VoucherServer.unVoucherAmount(offer.amount));

                    return (
                      <Offer
                        enabled={userCanBuy && offerInLimits}
                        theme={theme}
                        key={`${provider.id}-${offer.amount}`}
                        onClick={() =>
                          userCanBuy &&
                          initializeVoucherOrder([provider, offer])
                        }
                      >
                        <Logo src={provider.logoUrl} />
                        <VoucherText fontSize={1.25}>
                          {offer.amount}€ {provider.name}{' '}
                          {t('dashboard.voucherShop.voucher')}
                        </VoucherText>
                        <VoucherText fontSize={1}>
                          {offer.countAvailable}{' '}
                          {t('dashboard.voucherShop.vouchersLeft')}
                        </VoucherText>
                        <br />

                        {offerInLimits ? (
                          <VoucherText fontSize={1.5}>
                            {userCanBuy
                              ? `${t('dashboard.voucherShop.buyFor')} ${
                                  _VoucherServer.unVoucherAmount(offer.amount) *
                                  10
                                } Circles`
                              : `${t('dashboard.voucherShop.youNeed')} ${(
                                  _VoucherServer.unVoucherAmount(offer.amount) *
                                    10 -
                                  availableBalance
                                ).toFixed(2)} Circles`}
                          </VoucherText>
                        ) : (
                          <VoucherText fontSize={1.5}>
                            {t('dashboard.voucherShop.buyLimitReachedHint')}
                          </VoucherText>
                        )}
                      </Offer>
                    );
                  })}
                </OfferContainer>
              </div>
            );
          })}
        </>
      ) : (
        <SubClaim>{t('dashboard.voucherShop.buyNoVouchers')}</SubClaim>
      )}
    </>
  );
};

type OfferContainerProps = {
  elementCount: number;
};

const OfferContainer = styled.div<OfferContainerProps>(({ elementCount }) => [
  css`
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(15rem, 1fr));
    grid-gap: 1px;
  `,
]);

type OfferProps = {
  theme: Theme;
  enabled: boolean;
};

const Offer = styled.button<OfferProps>(({ theme, enabled }) => {
  return [
    css`
      cursor: ${enabled ? 'pointer' : 'not-allowed'};
      font-size: 2rem;
      color: ${theme.darkColor};
      background-color: ${theme.cardColor};
      padding: 1rem;
      ${enabled &&
      `&:hover {
        background-color: ${theme.textColorLight};
        color: ${theme.baseColor};
      }`}

      outline: 1px solid ${theme.darkColor};
      border: none;
    `,
  ];
});

// -----------------------------------------------------------------------------
// UI Logo
// -----------------------------------------------------------------------------

const Logo = styled.img(() => [
  css`
    max-width: 10rem;
  `,
]);

// -----------------------------------------------------------------------------
// UI Text
// -----------------------------------------------------------------------------

const VoucherText = styled.p<{ fontSize: number }>(({ fontSize }) => [
  css`
    font-size: ${fontSize}rem;
    margin: 0;
    padding: 0;
  `,
]);
