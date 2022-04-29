"use strict";

// module Gun.Sea

exports._create =
  function (gundb, name, pwd) { 
    return function (onError, onSuccess) { 
      var canceled = false;
      var user = gundb.user();
      user.create(function(data){
        if(!canceled) {
          onSuccess(user);
        }
      });
      return function (cancelError, cancelerError, cancelerSuccess) {
        canceled = true;
        cancelerSuccess();
      };
    };
  };

exports._auth = function (gundb, name, pwd) { 
  return function (onError, onSuccess) { 
    var canceled = false;
    var user = gundb.user();
    user.auth(function(data){
      if(!canceled) {
        onSuccess(user);
      }
    });
    return function (cancelError, cancelerError, cancelerSuccess) {
      canceled = true;
      cancelerSuccess();
    };
  };
};
