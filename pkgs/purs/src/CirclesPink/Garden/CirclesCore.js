"use strict";

var CirclesCore = require("@circles/core");
var Web3 = require("web3");

exports._newCirclesCore = (either) => (web3) => (options) => () => {
  try {
    return either.right(new CirclesCore(web3, options));
  } catch (e) {
    return either.left(e.message);
  }
};

exports.newWeb3 = (provider) => () => {
  return new Web3(provider);
};

exports._newWebSocketProvider = (either) => (url) => () => {
  try {
    return either.right(new Web3.providers.WebsocketProvider(url));
  } catch (e) {
    return either.left(e.message);
  }
};
