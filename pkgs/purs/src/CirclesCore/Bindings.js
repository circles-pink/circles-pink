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

exports.userResolveImpl =
  (circlesCore) => (account) => (options) => (onErr, onSucc) =>
    circlesCore.user.resolve(account, options).then(onSucc).catch(onErr);

exports.privKeyToAccount = (web3) => (privKey) => () =>
  web3.eth.accounts.privateKeyToAccount(privKey);

// CirclesCore - Safe

exports.safePredictAddress =
  (circlesCore) => (account) => (options) => (onErr, onSucc) => {
    circlesCore.safe.predictAddress(account, options).then(onSucc).catch(onErr);
  };

exports.safePrepareDeployImpl =
  (circlesCore) => (account) => (options) => (onErr, onSucc) => {
    circlesCore.safe.prepareDeploy(account, options).then(onSucc).catch(onErr);
  };

// CirclesCore - Trust

exports.trustGetNetwork =
  (circlesCore) => (account) => (options) => (onErr, onSucc) => {
    circlesCore.trust.getNetwork(account, options).then(onSucc).catch(onErr);
  };

// CirclesCore - unsafe debug

exports.unsafeSampleCore = (circlesCore) => (account) => (_, onSucc) => {
  window.sampleCore = circlesCore;
  window.sampleAccount = account;
  onSucc(null);
};
