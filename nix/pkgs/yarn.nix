{ pkgs, ... }:
rec {
  sources = pkgs.runCommand "mk-src" { } ''
    mkdir $out
    cp ${../../package.json} $out/package.json
    cp ${../../yarn.lock} $out/yarn.lock

    mkdir -p $out/pkgs/
    cp -r ${ pkgs.nix-gitignore.gitignoreSourcePure [../../.gitignore] ../../pkgs/ts} $out/pkgs/ts
  '';

  workspaces = pkgs.yarn2nix-moretea.mkYarnWorkspace {
    src = sources;
  };
}
