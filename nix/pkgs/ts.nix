{ pkgs, pursOutput, ... }: rec {

  localPackages = {
    dev-utils = ../../pkgs/ts/dev-utils;
    common = ../../pkgs/ts/common;
    storybook = ../../pkgs/ts/storybook;
    cli = ../../pkgs/ts/cli;
    # cli-playground = ../../pkgs/ts/cli-playground;
    circles = ../../pkgs/ts/circles;
  };

  src = pkgs.nix-filter.filter {
    root = ../../.;
    include = [
      "package.json"
      "yarn.lock"
    ] ++ (
      pkgs.lib.pipe localPackages [
        builtins.attrNames
        (map (name: "pkgs/ts/${name}/package.json"))
      ]
    )
    ;
  };

  emptyWorkspaces = pkgs.yarn2nix-moretea.mkYarnWorkspace {
    src = ../..;
  };

  replaceEmptyLocalDep = pkgName: pkgs.lib.pipe localPackages [
    (pkgs.lib.mapAttrsToList (depName: drv: ''
      dir="$tmp/libexec/${pkgName}/deps/${depName}"
      if [ -d $dir ];
      then
        tmpNodeModules=`mktemp -d`
        
        if [ -d "$dir/node_modules/" ];
          then cp -r $dir/node_modules/ $tmpNodeModules/node_modules
        fi
        
        rm -rf $dir
        cp -r ${drv} $dir
        chmod +w $dir
        rm -rf $dir/node_modules
        
        if [ -d "$tmpNodeModules/node_modules" ];
          then cp -r $tmpNodeModules/node_modules $dir/node_modules
          else cp -rf $dir/../../node_modules $dir/node_modules
        fi
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

  builds = {
    storybook = pkgs.runCommand "storybook" { buildInputs = [ pkgs.yarn ]; } ''
      tmp=`mktemp -d`
    
      mkdir -p $tmp/generated
      cp -r ${pursOutput} $tmp/generated/output

      mkdir -p $tmp/pkgs/ts

      cp ${../../package.json} $tmp/package.json

      cp -r ${../../pkgs/ts/storybook} $tmp/pkgs/ts/storybook
      chmod -R +w $tmp

      # cp -r ${workspaces.storybook}/libexec/storybook/node_modules/ \
      #   $tmp/node_modules

      ln -s ${workspaces.storybook}/libexec/storybook/node_modules $tmp/node_modules

      cp -r ${workspaces.storybook}/libexec/storybook/deps/storybook/node_modules/ \
        $tmp/pkgs/ts/storybook/node_modules

      chmod -R +w $tmp

      cd $tmp
      export OUTPUT_DIR=$tmp/dist; yarn workspace storybook run build

      cp -r $tmp $out
    '';
  };

}
