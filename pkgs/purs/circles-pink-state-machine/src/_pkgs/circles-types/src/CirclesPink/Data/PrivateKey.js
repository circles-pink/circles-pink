"use strict";

var ethWallet = require("ethereumjs-wallet").default;
var bip39 = require("bip39");

exports.genPrivateKeyImpl = function () {
  return ethWallet.generate().getPrivateKeyString();
};

exports.mnemonicToEntropyImpl = (Nothing) => (Just) => (mnemonic) => {
  try {
    const entropy = bip39.mnemonicToEntropy(mnemonic);
    return Just(entropy);
  } catch (error) {
    return Nothing;
  }
};
