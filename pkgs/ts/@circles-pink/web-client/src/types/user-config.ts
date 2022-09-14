import { ReactElement } from 'react';

export type UserConfig = {
  email?: string | ((email: string) => void);
  onTrackingEvent?: (json: unknown) => void;
  onTrackingResumee?: (json: unknown) => void;
  voucherShopEnabled: boolean;
  xbgeCampaign: boolean;
  sharingFeature: ReactElement | null;
  buyVoucherEurLimit: number;
  shadowFriends?: Array<string>;
  xbgeSafeAddress?: string;
};
