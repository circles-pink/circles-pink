"use strict";

var Web3 = require("web3");

exports.web3static = Web3;

exports.newWebSocketProvider = (url) => () =>
  new Web3.providers.WebsocketProvider(url);

exports.newWeb3 = (provider) => () => new Web3(provider);

exports.newWeb3_ = () => new Web3();

exports.privKeyToAccount = (web3) => (privKey) => () =>
  web3.eth.accounts.privateKeyToAccount(privKey);

exports.accountsSign = (web3) => (msg) => (pk) =>
  web3.eth.accounts.sign(msg, pk);

exports.accountsRecover = (web3) => (so) => () => web3.eth.accounts.recover(so);

exports.accountsHashMessage = (web3) => (msg) =>
  web3.eth.accounts.hashMessage(msg);

exports.sendTransaction = (web3) => (opts) =>
  function (onError, onSuccess) {
    var cancel = web3.eth.sendTransaction(opts, function (err, res) {
      if (err) {
        onError(err);
      } else {
        onSuccess(res);
      }
    });
    return function (cancelError, onCancelerError, onCancelerSuccess) {
      cancel();
      onCancelerSuccess();
    };
  };
