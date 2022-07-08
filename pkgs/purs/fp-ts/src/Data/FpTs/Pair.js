"use strict";

exports.mkPair = (x) => (y) => [x, y];

exports.unMkPair = (f) => ([x, y]) => f(x)(y);
