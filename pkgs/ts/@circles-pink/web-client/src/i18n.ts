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
        validation: {
          charFail: 'Only letters from A-Z and numbers from 0-9 are allowed!',
          lengthFail: 'Please use 3 - 24 letters.',
          availFail: 'Username not available',
        },
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
        getMoreTruts: 'Collect at least three trust connections',
        fundYourSafe: 'Or fund your Safe with xDai. Your Safe Address is:',
      },
      dashboard: {
        addTrustPlaceholder: 'Address of a person you want to trust',
        debugTitle: 'Dashboard actions for testing:',
        sendButton: 'Send',
        sendClaim: 'Send Circles',
        receiveButton: 'Receive',
        searchButton: 'Search',
        trustNetworkTitle: 'Trust Network',
        exploreTitle: 'Explore',
        trustList: {
          tableHead: {
            user: 'User',
            relation: 'Relation',
            action: 'Action',
          },
          untrust: 'Untrust {{user}}',
          trust: 'Trust {{user}}',
          send: 'Send to {{user}}',
          canNotSend: 'You can not send to {{user}}',
          relationSendable: '{{user}} accepts your Circles',
          relationNotSendable: '{{user}} does not accept your Circles',
          relationReceivable: 'You accept Circles from {{user}}',
          relationNotReceivable: 'You do not accept Circles from {{user}}',
        },
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
        claim: 'Probier das Grundeinkommen auf der Blockchain!',
        subClaim:
          'Um Teil des Circles Netzwerks zu werden, kannst du hier eine neue Wallet erstellen, oder dich einloggen, wenn du schon eine hast.',
      },
      login: {
        claim: 'Willkommen zurück!',
        subClaim:
          'Gib hier deinen privaten Schlüssel ein, um deine Circles Wallet wiederherzustellen.',
        magicWordsPlaceholder: 'Deine 24 geheimen Worte...',
      },
      infoGeneral: {
        claim: 'Schön, dass du dabei bist!',
        subClaim:
          'Lege eine neue Wallet an und werde Teil des Vertrauensnetzwerks!',
      },
      askUsername: {
        claim: 'Wie möchtest du dich nennen?',
        subClaim:
          'Das ist der Name, mit dem du im Circles Netzwerk gefunden wirst. Überlege gut, du kannst den Namen später nicht mehr ändern!',
        usernamePlaceholder: 'Dein einzigartiger Nutzername',
        validation: {
          charFail: 'Bitte nutze nur Buchstaben von A-Z und Zahlen von 0-9!',
          lengthFail: 'Bitte nutze mindestens 3 und maximal 24 Buchstaben.',
          availFail: 'Der Nutzer:innen-Name ist vergeben.',
        },
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
          'Dieser Schlüssel ist deine Eintrittskarte ins Circles Netzwerk. Damit kannst du dein Vertrauensnetzwerk aufbauen und als einer der ersten das Grundeinkommen auf der Blockchain testen!',
      },
      magicWords: {
        claim: 'Der eine Schlüssel...',
        subClaim1:
          'Schreibe dir diese Worte am besten auf ein Blatt Papier, oder speichere sie an einem sicheren Ort.',
        subClaim2:
          'Wenn du sie verlierst, dann kann dein Account nicht wiederhergestellt werden. All deine Vertrauensbeziehungen sind damit dann unzugänglich.',
        copyBtn: 'In die Zwischenablage kopieren',
        copiedInfo: 'Kopiert!',
        newPhraseBtn: 'Neuen Schlüssel generieren',
      },
      submit: {
        claim: 'Diese Daten hast du angegeben',
        subClaim:
          'Wenn alles stimmt kannst du sie abschicken. Damit ist dein Nutzername dann vorgemerkt. Wie du deinen Account aktivierst, erfährst du im nächsten Schritt.',
      },
      trusts: {
        claim: 'Hallöchen!',
        greet: 'Hallo',
        subClaim:
          'Dein Account ist noch nicht aktiviert. Da Interkationen mit deinem Vertrauensnetzwerk in die Blockchain geschrieben werden, was prinzipiell nicht kostenlos sind, brauchst du drei Trusts von Nutzer:innen, die bereits im Circles Netzwerk sind. Natürlich musst du diese Transaktionen nicht selber zahlen, aber es muss ausgeschlossen sein, dass Bots sich diverse Accounts anlegen.',
        getMoreTruts:
          'Sammle mindestens drei Trusts von Circles User:innen. Nenne ihnen dazu einfach deinen Nutzernamen, oder lass sie diesen Code scannen.',
        fundYourSafe:
          'Oder funde deine Wallet mit mindestens einem xDai. Das ist deine Safe Adresse, wenn sie mit mindestens einem Euro gefunded ist, kannst du die Registrierung abschließen.',
      },
      dashboard: {
        addTrustPlaceholder:
          'Adresse einer Person, die du in dein Vertauensnetzwerk hinzufügen willst',
        debugTitle: 'Dashboard actions zum ausprobieren:',
        sendButton: 'Senden',
        receiveButton: 'Empfangen',
        searchButton: 'Suchen',
        trustNetworkTitle: 'Trust Netzwerk',
        exploreTitle: 'Erkunden',
        sendClaim: 'Circles überweisen',
        trustList: {
          tableHead: {
            user: 'User:in',
            relation: 'Verbindung',
            action: 'Aktionen',
          },
          untrust: '{{user}} nicht mehr vertrauen',
          trust: 'Vertraue {{user}}',
          send: 'Circles an {{user}} senden',
          canNotSend: 'Du kannst keine Circles an {{user}} senden',
          relationSendable: '{{user}} akzeptiert deine Circles',
          relationNotSendable: '{{user}} akzeptiert deine Circles nicht',
          relationReceivable: 'Du akzeptierst Circles von {{user}}',
          relationNotReceivable: 'Du akzeptierst Circles von {{user}} nicht',
        },
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
