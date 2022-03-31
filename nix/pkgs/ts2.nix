{ pkgs, ... }:
let
  fetchTarball = x: pkgs.runCommand x.name { } ''
    mkdir $out
    tar -xf ${pkgs.fetchurl x} --directory $out
  '';
in
{
  yarnLockJson = pkgs.runCommand "yarn-lock.json" { buildInputs = [ pkgs.yarnLockToJson ]; }
    ''
      yarn-lock-to-json ${../../yarn.lock} > $out
    '';

  #yarnSources = builtins.mappAttribs (v: fetchTarball {}) yarnLockJson;

  yarnSource = fetchTarball {
    name = "a";
    url = "https://registry.yarnpkg.com/@aws-crypto/ie11-detection/-/ie11-detection-2.0.0.tgz#bb6c2facf8f03457e949dcf0921477397ffa4c6e";
    sha512 = "sha512-pkVXf/dq6PITJ0jzYZ69VhL8VFOFoPZLZqtU/12SGnzYuJOOGNfF41q9GxdI1yqC8R13Rq3jOLKDFpUJFT5eTA==";
  };
}
