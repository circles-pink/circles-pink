import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';

export const defaultNS = 'translations';
export const resources = {
  en: {
    translations: {
      nextButton: 'Next',
      prevButton: 'Back',
      submitButton: 'Submit',
      infoGeneral: {
        claim: 'Welcome to Circles',
        subClaim: "Let's get you a circles Wallet!",
      },
      askUsername: {
        claim: 'Tell me your Name!',
        subClaim: 'Choose wisely, you can not change it later!',
        usernamePlaceholder: 'Your amazing username',
      },
      askEmail: {
        claim: 'How do you want to be notified?',
        subClaim: 'Please submit a valid e-mail address.',
        emailPlaceholder: 'Enter your E-Mail',
        termsLabel: 'Accept terms',
        privacyLabel: 'Accept privacy',
      },
      infoSecurity: {
        claim: "Let's talk about Security",
        subClaim: 'Most important: Keep your seedphrase save!',
      },
      magicWords: {
        claim: 'One key to rule them all...',
        subClaim1:
          'Anyone with these words has full control over your Trustnetwork and can spend your $CRC!',
        subClaim2:
          'You should write this on a sheet of paper to keep your Coins save.',
        copyBtn: 'Copy to clipboard',
        copiedInfo: 'Copied!',
        newPhraseBtn: 'Generate new Phrase',
      },
      submit: {
        claim: 'This is your data',
        subClaim: 'If everything is correct you can submit them',
      },
    },
  },
  de: {
    translations: {
      nextButton: 'Weiter',
      prevButton: 'Zurück',
      submitButton: 'Abschicken',
      infoGeneral: {
        claim: 'Willkommen bei Circles',
        subClaim: 'Lege eine Wallet an und werde Teil des Vertrauensnetzwerks!',
      },
      askUsername: {
        claim: 'Wie möchtest du dich nennen?',
        subClaim: 'Überlege gut, du kannst den Namen später nicht mehr ändern!',
        usernamePlaceholder: 'Dein wunderschöner Nutzername',
      },
      askEmail: {
        claim: 'Wie möchtest du kontaktiert werden?',
        subClaim: 'Bitte gib eine gültige E-Mail Adresse an!',
        emailPlaceholder: 'Deine E-Mail Adresse',
        termsLabel: 'Stimme den Nutzungsbedingungen zu',
        privacyLabel: 'Akzeptiere die Datenschutzbestimmungen',
      },
      infoSecurity: {
        claim: 'Lass uns über Datensicherheit reden!',
        subClaim: 'Am wichtigsten ist, deinen Schlüssel gut aufzuheben.',
      },
      magicWords: {
        claim: 'Der eine Schlüssel...',
        subClaim1:
          'Wer diesen Schlüssel hat, hat volle Kontrolle über Dein Netzwerk und kann Deine $CRC ausgeben!',
        subClaim2:
          'Am besten schreibst Du diese Worte auf ein Stück Papier und hebst sie an einem sicheren Ort auf:',
        copyBtn: 'In die Zwischenablage kopieren',
        copiedInfo: 'Kopiert!',
        newPhraseBtn: 'Neuen Schlüssel generieren',
      },
      submit: {
        claim: 'Diese Daten hast du angegeben',
        subClaim: 'Wenn alles stimmt kannst du sie abschicken.',
      },
    },
  },
} as const;

i18n.use(initReactI18next).init({
  lng: 'en',
  ns: ['translations'],
  defaultNS,
  resources,
});

export default i18n;
