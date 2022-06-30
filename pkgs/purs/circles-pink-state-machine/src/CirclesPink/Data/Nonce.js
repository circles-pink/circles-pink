"use strict";

exports.addressToNonceImpl = function (address) {
  return parseInt(address.slice(30), 16);
};
