"use strict";

// Web3

var Web3 = require("web3");

var BN = require("bn.js");

exports.newWebSocketProvider = (url) => () =>
  new Web3.providers.WebsocketProvider(url);

exports.newWeb3 = (provider) => () => new Web3(provider);

exports.privKeyToAccount = (web3) => (privKey) => () =>
  web3.eth.accounts.privateKeyToAccount(privKey);

exports.sendTransaction = (web3) => (from) => (to) => () =>
  web3.eth.sendTransaction({ from, to, value: web3.utils.toWei("1", "ether") });

// CirclesCore

var CirclesCore = require("@circles/core");

exports.newCirclesCore = (web3) => (options) => () =>
  new CirclesCore(web3, options);

exports.mkCirclesCore = (web3) => (options) => () =>
  new CirclesCore(web3, options);

// CirclesCore - unsafe debug

exports.unsafeSampleCore = (circlesCore) => (account) => (_, onSucc) => {
  window.sampleCore = circlesCore;
  window.sampleAccount = account;
  window.BN = BN;
  onSucc(null);
};
