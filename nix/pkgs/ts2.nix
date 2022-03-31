{ pkgs, ... }:
let
  fetchTarball = x: pkgs.runCommand x.name { } ''
    mkdir $out
    tar -xf ${pkgs.fetchurl x} --directory $out
  '';
in
rec {
  yarnLockJson = pkgs.runCommand "yarn-lock.json" { buildInputs = [ pkgs.yarnLockToJson ]; }
    ''
      yarn-lock-to-json ${../../yarn.lock} > $out
    '';

  yarnLock = builtins.fromJSON (builtins.readFile yarnLockJson);

  yarnSources = builtins.mapAttrs
    (n: v: fetchTarball {
      name = builtins.replaceStrings [ "@" "/" ] [ "__at__" "__slash__" ] n;
      url = v.resolved;
      sha512 = v.integrity;
    })
    yarnLock;

  x = yarnSources.${''@aws-crypto/sha256-browser@2.0.0''};

}
