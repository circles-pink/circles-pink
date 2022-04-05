{ pkgs, ... }:
let

  mkYarnPkg = path:
    let
      src = pkgs.lib.cleanSource path;

      yarnPkg = pkgs.yarn2nix-moretea.mkYarnPackage {
        inherit src;
        packageJSON = src + "/package.json";
        yarnLock = src + "/yarn.lock";
      };

    in
    yarnPkg;

  mkYarnBins = yarnPkgs: name: cliNames:
    let
      linkCli = cliName: ''
        ln -s ${yarnPkgs.${name}}/libexec/${name}/node_modules/.bin/${cliName} $out/bin/${cliName}
      '';

      bins = pkgs.runCommand name { } ''
        mkdir -p $out/bin
        ${pkgs.lib.concatMapStringsSep "\n" linkCli cliNames}
      '';
    in
    bins;
in

rec {
  yarnPkgs = {
    "@circles-pink/chokidar-cli" = mkYarnPkg ./chokidar-cli;
    "@circles-pink/change-case" = mkYarnPkg ./change-case;
  };
  bins = {
    chokidar-cli = mkYarnBins yarnPkgs "@circles-pink/chokidar-cli" [ "chokidar" ];

  };
}
 