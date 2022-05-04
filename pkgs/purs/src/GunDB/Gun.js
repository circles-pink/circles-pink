"use strict";

// module Gun

var Gun = require("gun");

exports.syncWithPeer = function (url) {
  return function () {
    return Gun(url);
  };
};

exports._on = function (ctx) {
  return function (onError, onSuccess) {
    var canceled = false;
    ctx.on(function (data, key) {
      if (!canceled) {
        onSuccess({ data: data, key: key });
      }
    });
    return function (cancelError, cancelerError, cancelerSuccess) {
      canceled = true;
      cancelerSuccess();
    };
  };
};

exports.syncWithPeers = function (urls) {
  return function () {
    return Gun(urls);
  };
};

exports.offline = function () {
  return Gun();
};

exports._getOnGunDb = function (pathElements) {
  return function (ctx) {
    var arrayLength = pathElements.length;
    for (var i = 0; i < arrayLength; i++) {
      ctx = ctx.get(pathElements[i]);
    }
    return ctx;
  };
};

exports._getOnUser = function (pathElements) {
  return function (ctx) {
    var arrayLength = pathElements.length;
    for (var i = 0; i < arrayLength; i++) {
      ctx = ctx.get(pathElements[i]);
    }
    return ctx;
  };
};

exports.map = function (mapper) {
  return function (ctx) {
    return ctx.map(mapper);
  };
};

exports.each = function (ctx) {
  return ctx.map();
};

exports.filter = function (filter) {
  return function (ctx) {
    return ctx.map(function (e) {
      return filter(e) ? e : undefined;
    });
  };
};

exports.put = function (data) {
  return function (ctx) {
    return function () {
      return ctx.put(data);
    };
  };
};

exports.set = function (ref) {
  return function (ctx) {
    return function () {
      return ctx.set(ref);
    };
  };
};

exports._load = (Nothing) => (Just) => {
  return function (waittime) {
    return function (ctx) {
      return function (onError, onSuccess) {
        var canceled = false;
        ctx.load(
          function (data) {
            if (!canceled) {
              if (data === undefined) {
                onSuccess(Nothing);
              } else {
                onSuccess(Just(data));
              }
            }
          },
          { wait: waittime }
        );
        return function (cancelError, cancelerError, cancelerSuccess) {
          canceled = true;
          cancelerSuccess();
        };
      };
    };
  };
};

exports._once = (Nothing) => (Just) => {
  return function (ctx) {
    return function (onError, onSuccess) {
      var canceled = false;
      ctx.once(function (data, key) {
        if (!canceled) {
          if (data === undefined) {
            onSuccess(Nothing);
          } else {
            onSuccess(Just({ data: data, key: key }));
          }
        }
      });
      return function (cancelError, cancelerError, cancelerSuccess) {
        canceled = true;
        cancelerSuccess();
      };
    };
  };
};
