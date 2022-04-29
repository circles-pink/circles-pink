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

exports.isPrivateKeyImpl = function (str) {
  try {
    ethWallet.fromPrivateKey(ethUtils.toBuffer("0x" + str));
    return true;
  } catch (error) {
    return false;
  }
};

exports.addressToNonceImpl = function (address) {
  return parseInt(address.slice(30), 16);
};

exports.entropyToMnemonicImpl = function (entropy) {
  return bip39.entropyToMnemonic(entropy);
};

exports.mnemonicToEntropyImpl = function (mnemonic) {
  return bip39.mnemonicToEntropy(mnemonic);
};
