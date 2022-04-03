{ pkgs, ... }: rec {
  yarnPackages = {
    chokidar-cli = pkgs.yarn2nix-moretea.mkYarnPackage {
      src = pkgs.runCommand "src" { } ''
        mkdir $out
        ln -s ${./chokidar-cli/package.json} $out/package.json
      '';
      packageJSON = ./chokidar-cli/package.json;
      yarnLock = ./chokidar-cli/yarn.lock;
    };
  };

  bins = {
    chokidar-cli = pkgs.runCommand "chokidar-cli" { } ''
      mkdir -p $out/bin
      ln -s ${yarnPackages.chokidar-cli}/libexec/yarn2nix-chokidar-cli/node_modules/.bin/chokidar $out/bin/chokidar
    '';
  };
}
