"use strict";

var ethWallet = require("ethereumjs-wallet").default;

exports.genPrivateKeyImpl = function () {
  return ethWallet.generate().getPrivateKeyString();
};
