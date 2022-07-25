"use strict"

const BN = require('bn.js');

exports.fromStrImpl = (str) => (base) => new BN(str, base)

exports.toStrImpl = (bn) => (base) => bn.toString(base)

exports.eqImpl = (bn1) => (bn2) => bn1.eq(bn2)

exports.addImpl = (bn1) => (bn2) => bn1.add(bn2)

exports.mulImpl = (bn1) => (bn2) => bn1.mul(bn2)