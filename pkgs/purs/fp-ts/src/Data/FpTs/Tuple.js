"use strict";

exports.mkTuple = (x) => (y) => [x, y];

exports.unMkTuple = (f) => ([x, y]) => f(x)(y);
