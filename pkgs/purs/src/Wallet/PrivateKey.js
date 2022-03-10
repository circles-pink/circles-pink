"use strict";

var ethWallet = require("ethereumjs-wallet").default;
var ethUtils = require("ethereumjs-util");
var bip39 = require("bip39");

exports.genPrivateKeyImpl = function () {
  return ethWallet.generate().getPrivateKeyString();
};

exports.privKeyToAddressImpl = function (privKey) {
  return ethWallet
    .fromPrivateKey(ethUtils.toBuffer(privKey))
    .getAddressString();
};

exports.entropyToMnemonicImpl = function (entropy) {
  return bip39.entropyToMnemonic(entropy);
};

exports.mnemonicToEntropyImpl = function (mnemonic) {
  return bip39.mnemonicToEntropy(mnemonic);
};
