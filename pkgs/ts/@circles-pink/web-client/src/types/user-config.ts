import { ReactElement } from 'react';

export type Language = 'en' | 'de';

export type Content = {};

export type UserConfig = {
  lang?: Language;
  content?: Content;
  email?: string | ((email: string) => void);
  onTrackingEvent?: (json: unknown) => void;
  onTrackingResumee?: (json: unknown) => void;
  voucherShopEnabled?: boolean;
  xbgeCampaign?: boolean;
  sharingFeature?: ReactElement | null;
  buyVoucherEurLimit?: number;
  shadowFriends?: Array<string>;
  safeAddress?: string | null;
  xbgeSafeAddress?: string | null;
  strictMode?: boolean;
  testEnv?: boolean;
};
