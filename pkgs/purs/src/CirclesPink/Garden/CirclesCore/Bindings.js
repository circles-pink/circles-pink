"use strict";

var CirclesCore = require("@circles/core");
var Web3 = require("web3");

exports.newWebSocketProvider = (url) => () =>
  new Web3.providers.WebsocketProvider(url);

exports.newWeb3 = (provider) => () => new Web3(provider);

exports.newCirclesCore = (web3) => (options) => () =>
  new CirclesCore(web3, options);

exports.userRegisterImpl =
  (circlesCore) => (account) => (options) => (onErr, onSucc) =>
    circlesCore.user.register(account, options).then(onSucc).catch(onErr);

exports.privKeyToAccount = (web3) => (privKey) => () =>
  web3.eth.accounts.privateKeyToAccount(privKey);

exports.safePredictAddressImpl =
  (circlesCore) => (account) => (options) => (onErr, onSucc) => {
    circlesCore.safe.predictAddress(account, options).then(onSucc).catch(onErr);
  };
