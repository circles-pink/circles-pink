"use strict";

var bip39 = require("bip39");

exports.mnemonicToEntropyImpl = (Nothing) => (Just) => (mnemonic) => {
  try {
    const entropy = bip39.mnemonicToEntropy(mnemonic);
    return Just(entropy);
  } catch (error) {
    return Nothing;
  }
};

exports.entropyToMnemonicImpl = function (entropy) {
  return bip39.entropyToMnemonic(entropy.slice(2));
};
