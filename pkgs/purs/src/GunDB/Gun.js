"use strict";

// module Gun

var Gun = require("gun");

exports.syncWithPeer = (url) => () => Gun(url);

exports._on = (ctx) => (onErr, onSucc) => {
  try {
    ctx.on((data, key) => {
      onSucc({ data: data, key: key });
    });
  } catch (error) {
    onErr(error);
  }
};

exports.syncWithPeers = (urls) => () => Gun(urls);

exports.offline = () => Gun();

exports._getOnGunDb = (pathElements) => (ctx) => {
  var arrayLength = pathElements.length;
  for (var i = 0; i < arrayLength; i++) {
    ctx = ctx.get(pathElements[i]);
  }
  return ctx;
};

exports._getOnUser = (pathElements) => (ctx) => {
  var arrayLength = pathElements.length;
  for (var i = 0; i < arrayLength; i++) {
    ctx = ctx.get(pathElements[i]);
  }
  return ctx;
};

exports.map = (mapper) => (ctx) => ctx.map(mapper);

exports.each = (ctx) => ctx.map();

exports.filter = (filter) => (ctx) =>
  ctx.map((e) => (filter(e) ? e : undefined));

exports.put = (data) => (ctx) => () => ctx.put(data);

exports.set = (ref) => (ctx) => () => ctx.set(ref);

exports._once = (Nothing) => (Just) => (ctx) => (onErr, onSucc) => {
  ctx.once((data, key) => {
    try {
      if (data === undefined) {
        onSucc(Nothing);
      } else {
        onSucc(Just({ data: data, key: key }));
      }
    } catch (error) {
      onErr(error);
    }
  });
};

exports._load =
  (Nothing) => (Just) => (waittime) => (ctx) => (onErr, onSucc) => {
    try {
      ctx.load(
        (data) => {
          if (data === undefined) {
            onSucc(Nothing);
          } else {
            onSucc(Just(data));
          }
        },
        { wait: waittime }
      );
    } catch (error) {
      onErr(error);
    }
  };
