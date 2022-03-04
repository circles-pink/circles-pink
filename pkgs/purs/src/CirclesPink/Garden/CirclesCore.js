"use strict";

var CirclesCore = require("@circles/core");
var Web3 = require("web3");

exports.newCirclesCore = function (web3) {
  return function (options) {
    return function () {
      return new CirclesCore(web3, options);
    };
  };
};

exports.newWeb3 = function (provider) {
  return function () {
    return new Web3(provider);
  };
};

exports.newWebSocketProvider = function (url) {
  return function () {
    return new Web3.providers.WebsocketProvider(url);
  };
};
