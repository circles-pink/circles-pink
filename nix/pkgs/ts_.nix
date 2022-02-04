{ pkgs, ... }: rec {
  src = pkgs.nix-filter.filter {
    root = ../../.;
    include = [
      "package.json"
      "yarn.lock"
      "pkgs/ts_/cli/package.json"
      "pkgs/ts_/storybook/package.json"
      "pkgs/ts_/common/package.json"
    ];
  };

  localPackages = {
    common = ../../pkgs/ts_/common;
    storybook = ../../pkgs/ts_/storybook;
    cli = ../../pkgs/ts_/cli;
  };

  emptyWorkspaces = pkgs.yarn2nix-moretea.mkYarnWorkspace {
    inherit src;
  };

  replaceEmptyLocalDep = pkgName: pkgs.lib.pipe localPackages [
    (pkgs.lib.mapAttrsToList (depName: drv: ''
      dir="$tmp/libexec/${pkgName}/deps/${depName}"
      if [ -d $dir ];
      then
        rm -rf $dir
        cp -r ${drv} $dir
      fi
    ''))
    (builtins.concatStringsSep "\n")
  ];

  workspaces =
    builtins.mapAttrs
      (name: value: pkgs.runCommand "" { } ''
        tmp=`mktemp -d`
        cp -r ${value}/* -t $tmp
        chmod -R +w $tmp

        ${replaceEmptyLocalDep name}

        cp -r $tmp $out
      '')
      emptyWorkspaces;

}
