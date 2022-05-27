"use strict";

var Web3 = require("web3");

exports.web3static = Web3;

exports.newWebSocketProvider = (url) => () =>
  new Web3.providers.WebsocketProvider(url);

exports.newWeb3 = (provider) => () => new Web3(provider);

exports.privKeyToAccount = (web3) => (privKey) => () =>
  web3.eth.accounts.privateKeyToAccount(privKey);

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
