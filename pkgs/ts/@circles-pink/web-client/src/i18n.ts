import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';

export const defaultNS = 'translations';
export const resources = {
  en: {
    translations: {
      nextButton: 'Next',
      prevButton: 'Back',
      submitButton: 'Submit',
      signUpButton: 'Sign Up',
      signInButton: 'Sign In',
      signUpInsteadButton: 'SignUp instead',
      finalizeButton: 'Finalize Account',
      safeStateButton: 'Get Safe State',
      getTrustsButton: 'Get Trust Network',
      addTrustsButton: 'Trust',
      debugButton: 'Core to window Object',
      landing: {
        claim: 'Welcome to Circles',
        subClaim: 'Create a new Wallet or restore your existing one.',
      },
      login: {
        claim: 'Welcome back',
        subClaim: 'Enter your seedphrase to restore your Circles Wallet!',
        magicWordsPlaceholder: 'Your 24 secret words go here...',
      },
      infoGeneral: {
        claim: 'Happy to have you here!',
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
        subClaim:
          'If everything is correct you can hit submit and create your Wallet',
      },
      trusts: {
        claim: 'Hey there!',
        greet: 'Hello',
        subClaim:
          'Your Account is in pending state. Follow the instructions to finalize it!',
      },
      dashboard: {
        claim: 'Hey there!',
        greet: 'Hello',
        subClaim:
          'This is your Dashboard, grow your trust network or pay with Circles!',
        addTrustPlaceholder: 'Address of a person you want to trust',
        debugTitle: 'Dashboard actions for testing:',
      },
      debug: {
        claim: 'Internal tools',
        subClaim:
          'Attach a sampleCore or sampleAccount Instance to the browser console.',
        magicWordsPlaceholder: 'Circles Core private key - empty for default',
      },
    },
  },
  de: {
    translations: {
      nextButton: 'Weiter',
      prevButton: 'Zurück',
      submitButton: 'Abschicken',
      signUpButton: 'Wallet erstellen',
      signInButton: 'Anmelden',
      signUpInsteadButton: 'Neue Wallet erstellen',
      finalizeButton: 'Account erstellen',
      safeStateButton: 'Safe State abfragen',
      getTrustsButton: 'Vertrauensnetzwerk abfragen',
      addTrustsButton: 'Vertrauen',
      debugButton: 'Core ans window Object',
      landing: {
        claim: 'Willkommen bei Circles',
        subClaim:
          'Erstelle eine neue Wallet oder stelle eine bestehende wieder her.',
      },
      login: {
        claim: 'Willkommen zurück!',
        subClaim:
          'Gib hier deinen privaten Schlüssel ein, um deine Circles Wallet wiederherzustellen.',
        magicWordsPlaceholder: 'Deine 24 geheimen Worte...',
      },
      infoGeneral: {
        claim: 'Schön, dass du dabei bist!',
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
        claim: 'Gleich erhälst du deinen persönlichen Schlüssel',
        subClaim:
          'Damit hast du Zugang zum Circles Vertrauensnetzwerk und kannst als einer der ersten das Grundeinkommen auf der Blockchain testen!',
      },
      magicWords: {
        claim: 'Der eine Schlüssel...',
        subClaim1:
          'Schreibe dir diese Worte am besten auf ein Blatt Papier, oder speichere sie an einem sicheren Ort.',
        subClaim2: 'Damit kannst du deine Wallet immer wiederherstellen.',
        copyBtn: 'In die Zwischenablage kopieren',
        copiedInfo: 'Kopiert!',
        newPhraseBtn: 'Neuen Schlüssel generieren',
      },
      submit: {
        claim: 'Diese Daten hast du angegeben',
        subClaim: 'Wenn alles stimmt kannst du sie abschicken.',
      },
      trusts: {
        claim: 'Hallöchen!',
        greet: 'Hallo',
        subClaim:
          'Dein Account ist noch nicht aktiviert. Folge den Anweisungen um ihn freizuschalten!',
      },
      dashboard: {
        claim: 'Hallöchen!',
        greet: 'Hallo',
        subClaim:
          'Dies ist dein Dashboard. Bau dein Netzwerk aus oder zahle mit Circles!',
        addTrustPlaceholder:
          'Adresse einer Person, die du in dein Vertauensnetzwerk hinzufügen willst',
        debugTitle: 'Dashboard actions zum ausprobieren:',
      },
      debug: {
        claim: 'Interne tools',
        subClaim:
          'Eine sampleCore und sampleAccount Instanz in der Browser Console verfügbar machen.',
        magicWordsPlaceholder:
          'Circles Core privater Schlüssel - leer für default',
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
