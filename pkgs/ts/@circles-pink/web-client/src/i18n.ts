import i18n from 'i18next';
import { initReactI18next } from 'react-i18next';

export const defaultNS = 'translations';
export const resources = {
  en: {
    translations: {
      nextButton: 'Next',
      prevButton: 'Back',
      submitButton: 'Submit',
      signUpButton: 'Create Wallet',
      signInButton: 'Restore Wallet',
      signUpInsteadButton: 'Create Wallet instead',
      signInSubmitButton: 'Restore Wallet',
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
        validation: {
          invalidMnemonic: 'This private key is not valid.',
          userNotFound: 'No user found for this private key!',
        },
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
        validation: {
          email: 'Please enter a valid email address.',
          terms: 'Please accept terms.',
          privacy: 'Please accept privacy.',
        },
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
        getMoreTruts:
          'Collect at least three trust connections of existing Circles users.',
        fundYourSafe:
          'Fund your Safe with at least one xDai. Your Safe Address is:',
        collectTrustsTitle: 'Collect three trusts...',
        fundSafeTitle: 'Feeling generous today!',
        fundMySafeManually: 'I know what I am doing and I want to fund my safe',
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
        userSearchPlaceholder: 'Search by username',
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
          message: {
            loadingTrust: 'Trusting ...',
            loadingUntrust: 'Untrusting ...',
            pendingTrust: 'Confirming ...',
            pendingUntrust: 'Confirming ...',
          },
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
      prevButton: 'Zur??ck',
      submitButton: 'Abschicken',
      signUpButton: 'Neue Wallet erstellen',
      signInButton: 'Bestehende Wallet ??ffnen',
      signUpInsteadButton: 'Neue Wallet erstellen',
      signInSubmitButton: 'Wiederherstellen',
      finalizeButton: 'Account erstellen',
      safeStateButton: 'Safe State abfragen',
      getTrustsButton: 'Vertrauensnetzwerk abfragen',
      addTrustsButton: 'Vertrauen',
      debugButton: 'Core ans window Object',
      landing: {
        claim: 'Probier das Grundeinkommen auf der Blockchain!',
        subClaim:
          'Um Teil des Circles Netzwerks zu werden, kannst du hier eine neue Wallet erstellen, oder wiederherstellen, wenn du schon eine hast.',
      },
      login: {
        claim: 'Willkommen zur??ck!',
        subClaim:
          'Gib hier deinen privaten Schl??ssel ein, um deine Circles Wallet wiederherzustellen. Dein privater Schl??ssel besteht aus 24 Worten, durch Leerzeichen getrennt.',
        magicWordsPlaceholder: 'Deine 24 geheimen Worte...',
        validation: {
          invalidMnemonic: 'Bitte pr??fe deinen Privaten Schl??ssel!',
          userNotFound: 'Zu diesem Schl??ssel existiert kein:e User:in.',
        },
      },
      infoGeneral: {
        claim: 'Sch??n, dass du dabei bist!',
        subClaim:
          'Lege eine neue Wallet an und werde Teil des Vertrauensnetzwerks!',
      },
      askUsername: {
        claim: 'Wie m??chtest du dich nennen?',
        subClaim:
          'Das ist der Name, mit dem du im Circles Netzwerk gefunden wirst. ??berlege gut, du kannst den Namen sp??ter nicht mehr ??ndern!',
        usernamePlaceholder: 'Dein einzigartiger Nutzername',
        validation: {
          charFail: 'Bitte nutze nur Buchstaben und Zahlen!',
          lengthFail: 'Bitte nutze 3 - 24 Zeichen.',
          availFail: 'Der Nutzer:innen-Name ist vergeben.',
        },
      },
      askEmail: {
        claim: 'Wie m??chtest du kontaktiert werden?',
        subClaim: 'Bitte gib eine g??ltige E-Mail Adresse an!',
        emailPlaceholder: 'Deine E-Mail Adresse',
        termsLabel: 'Stimme den Nutzungsbedingungen zu',
        privacyLabel: 'Akzeptiere die Datenschutzbestimmungen',
        validation: {
          email: 'Bitte gib eine g??ltige E-mail an.',
          terms: '* Pflichtfeld',
          privacy: '* Pflichtfeld',
        },
      },
      infoSecurity: {
        claim: 'Gleich erh??ltst du deinen pers??nlichen Schl??ssel',
        subClaim:
          'Dieser Schl??ssel ist deine Eintrittskarte ins Circles Netzwerk. Damit kannst du dein Vertrauensnetzwerk aufbauen und als eine:r der Ersten das Grundeinkommen auf der Blockchain testen!',
      },
      magicWords: {
        claim: 'Der eine Schl??ssel...',
        subClaim1:
          'Schreibe dir diese Worte am besten auf ein Blatt Papier, oder speichere sie an einem sicheren Ort.',
        subClaim2:
          'Wenn du sie verlierst, dann kann dein Account nicht wiederhergestellt werden. All deine Vertrauensbeziehungen sind damit dann unzug??nglich.',
        copyBtn: 'In die Zwischenablage kopieren',
        copiedInfo: 'Kopiert!',
        newPhraseBtn: 'Neuen Schl??ssel generieren',
      },
      submit: {
        claim: 'Diese Daten hast du angegeben',
        subClaim:
          'Wenn alles stimmt kannst du sie abschicken. Damit ist dein Nutzername dann vorgemerkt. Wie du deinen Account aktivierst, erf??hrst du im n??chsten Schritt.',
      },
      trusts: {
        claim: 'Hall??chen!',
        greet: 'Hallo',
        subClaim: 'Dein Account ist noch nicht aktiviert.',
        getMoreTruts:
          'Sammle mindestens drei Trusts von Circles User:innen. Nenne ihnen dazu einfach deinen Nutzernamen. Danach kannst du deinen Account freischalten und erh??lst Grundeinkommen!',
        fundYourSafe:
          'Oder funde deine Wallet mit mindestens einem xDai. Das ist deine Safe Adresse, wenn sie mit mindestens einem Euro gefunded ist, kannst du die Registrierung abschlie??en.',
        collectTrustsTitle: 'Sammle drei Trusts!',
        fundSafeTitle: 'Funde deinen Safe!',
        fundMySafeManually:
          'Ich wei?? was ich tue und m??chte meinen Safe selbst funden.',
      },
      dashboard: {
        addTrustPlaceholder:
          'Adresse einer Person, die du in dein Vertauensnetzwerk hinzuf??gen willst',
        debugTitle: 'Dashboard actions zum ausprobieren:',
        sendButton: 'Senden',
        receiveButton: 'Empfangen',
        searchButton: 'Suchen',
        trustNetworkTitle: 'Trust Netzwerk',
        exploreTitle: 'Erkunden',
        userSearchPlaceholder: 'Nutzer:innen suchen',
        sendClaim: 'Circles ??berweisen',
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
          message: {
            loadingTrust: 'Trusten ...',
            loadingUntrust: 'Untrusten ...',
            pendingTrust: 'Best??tige ...',
            pendingUntrust: 'Best??tige ...',
          },
        },
      },
      debug: {
        claim: 'Interne tools',
        subClaim:
          'Eine sampleCore und sampleAccount Instanz in der Browser Console verf??gbar machen.',
        magicWordsPlaceholder:
          'Circles Core privater Schl??ssel - leer f??r default',
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
