"use strict";

exports.newEs = (secretKey) => (options) => () =>
  new EncryptedStorage(secretKey, options);

exports.setItem = (es) => (key) => (value) => () => es.setItem(key, value);

exports.getItem = (Nothing) => (Just) => (es) => (key) => () => {
  const value = es.getItem(key);
  if (value === undefined) return Nothing;
  return Just(value);
};

exports.removeItem = (es) => (key) => () => es.removeItem(key);
