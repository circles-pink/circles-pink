"use strict";

var web3 = require("web3");

exports.web3 = web3;

exports.sendTransaction = (opts) =>
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
