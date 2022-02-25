"use strict";

var ethWallet = require("ethereumjs-wallet").default;
var bip39 = require("bip39");

exports.genPrivateKeyImpl = function () {
  return ethWallet.generate().getPrivateKeyString();
};

exports.entropyToMnemonicImpl = function (entropy) {
  return bip.entropyToMnemonic(entropy);
};

exports.mnemonicToEntropyImpl = function (mnemonic) {
  return bip.mnemonicToEntropy(mnemonic);
};
