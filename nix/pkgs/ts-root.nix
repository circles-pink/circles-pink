{ pkgs, ... }:
rec {
  workspaceRoot = pkgs.yarn2nix-moretea.mkYarnPackage {
    src = pkgs.runCommand "src" { } ''
      mkdir $out
      ln -s ${../../package.json} $out/package.json
    '';
    packageJSON = ../../package.json;
    yarnLock = ../../yarn.lock;
  };

  bins = {
    cspell = pkgs.runCommand "cspell" { } ''
      mkdir -p $out/bin
      ln -s ${workspaceRoot}/libexec/circles-pink/node_modules/.bin/cspell $out/bin/cspell
    '';
    ts-node = pkgs.runCommand "ts-node" { } ''
      mkdir -p $out/bin
      ln -s ${workspaceRoot}/libexec/circles-pink/node_modules/.bin/ts-node $out/bin/ts-node
    '';
  };

  default = bins;
}
