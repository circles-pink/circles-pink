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
        subClaim: 'Create a new Circles account (Wallet).',
        loginWithKey:
          'If you already have a secret key, you can restore your wallet here:',
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
        newPhraseBtn: 'Give me different words',
      },
      submit: {
        claim: 'Register user {{user}}',
        subClaim:
          'If you like your username and saved your private key, hit Submit and register your Circles account!',
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
        voucherShop: {
          shopTitle: 'Voucher Shop',
          buyDescription: 'Buy Vouchers:',
          buyNoVouchers: 'Currently there were no vouchers available.',
          listDescription: 'Your Vouchers:',
          listNoVouchers: "You don't have any vouchers yet.",
          confirmSendClaim: 'Confirm your purchase',
          buyFrom: 'Buy voucher from:',
          tcCostWillBe: 'You will be charged:',
          eurAmountWillBe: 'The voucher amount is:',
          confirmSendButton: 'Confirm',
          confirmSendProblem:
            'Sorry, we could not process your purchase, please try again later!',
          buyFor: 'Buy for',
          youNeed: 'You need',
          waitingFor: 'Waiting for',
          vouchersLeft: 'Vouchers left!',
          voucher: 'voucher',
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
      signUpButton: 'Neue Wallet erstellen',
      signInButton: 'Bestehende Wallet öffnen',
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
          'Um Teil des Circles Netzwerks zu werden, kannst du hier einen neuen Circles Account (Wallet) erstellen.',
        loginWithKey:
          'Wenn du schon einen Account hast, stelle deine Wallet hier wieder her:',
      },
      login: {
        claim: 'Willkommen zurück!',
        subClaim:
          'Gib hier deinen privaten Schlüssel ein, um deine Circles Wallet wiederherzustellen. Dein privater Schlüssel besteht aus 24 Worten, durch Leerzeichen getrennt.',
        magicWordsPlaceholder: 'Deine 24 geheimen Worte...',
        validation: {
          invalidMnemonic: 'Bitte prüfe deinen Privaten Schlüssel!',
          userNotFound: 'Zu diesem Schlüssel existiert kein:e User:in.',
        },
      },
      askUsername: {
        claim: 'Wie möchtest du dich nennen?',
        subClaim:
          'Das ist der Name, mit dem du im Circles Netzwerk gefunden wirst. Achtung, du kannst deinen Namen später nicht mehr ändern!',
        usernamePlaceholder: 'Dein einzigartiger Nutzername',
        validation: {
          charFail: 'Bitte nutze nur Buchstaben und Zahlen!',
          lengthFail: 'Bitte nutze 3 - 24 Zeichen.',
          availFail: 'Der Nutzer:innen-Name ist vergeben.',
        },
      },
      askEmail: {
        claim: 'Wie möchtest du kontaktiert werden?',
        subClaim: 'Bitte gib eine gültige E-Mail Adresse an!',
        emailPlaceholder: 'Deine E-Mail Adresse',
        termsLabel: 'Stimme den Nutzungsbedingungen zu',
        privacyLabel: 'Akzeptiere die Datenschutzbestimmungen',
        validation: {
          email: 'Bitte gib eine gültige E-mail an.',
          terms: '* Pflichtfeld',
          privacy: '* Pflichtfeld',
        },
      },
      infoSecurity: {
        claim: 'Gleich erhältst du deinen persönlichen Schlüssel',
        subClaim:
          'Dieser Schlüssel ist deine Eintrittskarte ins Circles Netzwerk. Damit kannst du dein Vertrauensnetzwerk aufbauen und als eine:r der Ersten das Grundeinkommen auf der Blockchain testen!',
      },
      magicWords: {
        claim: 'Der eine Schlüssel...',
        subClaim1:
          'Schreibe dir diese Worte am besten auf ein Blatt Papier, oder speichere sie an einem sicheren Ort.',
        subClaim2:
          'Wenn du sie verlierst, dann kann dein Account nicht wiederhergestellt werden. All deine Vertrauensbeziehungen sind damit dann unzugänglich.',
        copyBtn: 'In die Zwischenablage kopieren',
        copiedInfo: 'Kopiert!',
        newPhraseBtn: 'Gib mir schönere Worte!',
      },
      submit: {
        claim: 'Nutzer:in {{user}} anlegen',
        subClaim:
          'Wenn dein Name dir gefällt und du deinen privaten Schlüssel gespeichert hast, kannst du die Daten abschicken. Damit ist dein Nutzer:innenname vorgemerkt. Um die Aktivierung abzuschließen, musst du im nächsten Schritt drei Menschen finden, die dir vertrauen.',
      },
      trusts: {
        claim: 'Hallöchen!',
        greet: 'Hallo',
        subClaim: 'Dein Account ist noch nicht aktiviert.',
        getMoreTruts:
          'Sammle mindestens drei Trusts von Circles User:innen. Nenne ihnen dazu einfach deinen Nutzernamen. Danach kannst du deinen Account freischalten und erhälst Grundeinkommen!',
        fundYourSafe:
          'Oder funde deine Wallet mit mindestens einem xDai. Das ist deine Safe Adresse, wenn sie mit mindestens einem Euro gefunded ist, kannst du die Registrierung abschließen.',
        collectTrustsTitle: 'Sammle drei Trusts!',
        fundSafeTitle: 'Funde deinen Safe!',
        fundMySafeManually:
          'Ich weiß was ich tue und möchte meinen Safe selbst funden.',
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
        userSearchPlaceholder: 'Nutzer:innen suchen',
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
          message: {
            loadingTrust: 'Trusten ...',
            loadingUntrust: 'Untrusten ...',
            pendingTrust: 'Bestätige ...',
            pendingUntrust: 'Bestätige ...',
          },
        },
        voucherShop: {
          shopTitle: 'Gutschein Shop',
          buyDescription: 'Gutscheine kaufen:',
          buyNoVouchers:
            'Im Moment gibt es keine Gutscheine, versuche es später nochmal!',
          listDescription: 'Deine Gutscheine:',
          listNoVouchers: 'Du hast noch keine Gutscheine gekauft.',
          confirmSendClaim: 'Bestätige deine Transaktion',
          buyFrom: 'Gutschein kaufen von:',
          tcCostWillBe: 'Wir berechnen dir:',
          eurAmountWillBe: 'Der Wert des Gutscheines ist:',
          confirmSendButton: 'Kauf abschließen',
          confirmSendProblem:
            'Leider konnten wir deinen Kauf nicht abschließen. Bitte versuche es später nochmal!',
          buyFor: 'Kaufen für',
          youNeed: 'Dir fehlen',
          waitingFor: 'Warte auf',
          vouchersLeft: 'Gutscheine übrig!',
          voucher: 'Gutschein',
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
