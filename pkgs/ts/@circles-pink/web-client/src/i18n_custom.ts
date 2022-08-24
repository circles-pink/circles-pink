import i18n, { Resource } from 'i18next';
import { initReactI18next } from 'react-i18next';

export const defaultNS = 'translations';

export const mkI18n = (resources: Resource) => {
  i18n.use(initReactI18next).init({
    lng: 'de',
    ns: ['translations'],
    defaultNS,
    resources,
  });

  return i18n;
};
