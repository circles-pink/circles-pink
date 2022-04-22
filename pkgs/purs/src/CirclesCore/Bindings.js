"use strict";

var CirclesCore = require("@circles/core");
var Web3 = require("web3");

exports.newWebSocketProvider = (url) => () =>
  new Web3.providers.WebsocketProvider(url);

exports.newWeb3 = (provider) => () => new Web3(provider);

exports.newCirclesCore = (web3) => (options) => () =>
  new CirclesCore(web3, options);

exports.mkCirclesCore = (web3) => (options) => () =>
  new CirclesCore(web3, options);

exports.privKeyToAccount = (web3) => (privKey) => () =>
  web3.eth.accounts.privateKeyToAccount(privKey);

// CirclesCore - unsafe debug

exports.unsafeSampleCore = (circlesCore) => (account) => (_, onSucc) => {
  window.sampleCore = circlesCore;
  window.sampleAccount = account;
  onSucc(null);
};
