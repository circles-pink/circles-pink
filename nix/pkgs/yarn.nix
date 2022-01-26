{ pkgs, ... }:
rec {
  sources = pkgs.runCommand "mk-src" { } ''
    mkdir $out
    cp ${../../package.json} $out/package.json
    cp ${../../yarn.lock} $out/yarn.lock

    mkdir -p $out/pkgs/
    cp -r ${ pkgs.nix-gitignore.gitignoreSourcePure [../../.gitignore] ../../pkgs/ts} $out/pkgs/ts
  '';

  workspaceRoot = pkgs.yarn2nix-moretea.mkYarnPackage {
    src = pkgs.runCommand "src" { } ''
      mkdir $out
      ln -s ${../../package.json} $out/package.json
    '';
    packageJSON = ../../package.json;
    yarnLock = ../../yarn.lock;
  };

  workspaces = pkgs.yarn2nix-moretea.mkYarnWorkspace {
    src = sources;
  };

  bins = {
    cspell = pkgs.runCommand "" { } ''
      mkdir -p $out/bin
      ln -s ${workspaceRoot}/libexec/teal/node_modules/.bin/cspell $out/bin/cspell
    '';
  };

  builds = {
    storybook = pkgs.runCommand "storybook" { } ''
      mkdir $out
      ${pkgs.yarn}/bin/yarn \
        --cwd ${workspaces.storybook}/libexec/storybook/deps/storybook \
        build \
        --output-path $out
    '';
  };
}
