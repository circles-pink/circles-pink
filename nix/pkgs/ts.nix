{ pkgs, pursOutput, assets, zeus-client, ... }: rec {

  localPackages = builtins.mapAttrs (_: pkgs.lib.cleanSource)
    {
      dev-utils = ../../pkgs/ts/dev-utils;
      common = ../../pkgs/ts/common;
      storybook = ../../pkgs/ts/storybook;
      cli = ../../pkgs/ts/cli;
      circles = ../../pkgs/ts/circles;
      circles-directus = ../../pkgs/ts/circles-directus;
      generated = ../../pkgs/ts/generated;
      assets = ../../pkgs/ts/assets;
      tasks-explorer = ../../pkgs/ts/tasks-explorer;
      seed-db = ../../pkgs/ts/seed-db;
      tasks-explorer-server = ../../pkgs/ts/tasks-explorer-server;
      "zeus-client" = ../../pkgs/ts/zeus-client;
      "content" = ../../pkgs/ts/content;
    };

  src = pkgs.nix-filter.filter {
    root = pkgs.lib.cleanSource ../../.;
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
    inherit src;
  };

  replaceEmptyLocalDep = pkgName: pkgs.lib.pipe localPackages [
    (pkgs.lib.mapAttrsToList (depName: drv:
      let

        depName' = pkgs.lib.pipe "${drv}/package.json" [
          builtins.readFile
          builtins.fromJSON
          (x: x.name)
        ];

        extraLevel =
          if builtins.length (builtins.split "/" depName') == 1 then
            ""
          else
            "../";

      in
      ''
        dir="$tmp/libexec/${pkgName}/deps/${depName'}"
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
            else cp -rf $dir/../../${extraLevel}node_modules $dir/node_modules
          fi

          chmod -R +w $tmpNodeModules
          rm -rf $tmpNodeModules
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

        chmod -R +w $tmp
        rm -rf $tmp
      '')
      emptyWorkspaces;

  bins = {
    depcruise = pkgs.writeShellScriptBin "depcruise" ''
      ${workspaces.dev-utils}/libexec/dev-utils/node_modules/dependency-cruiser/bin/dependency-cruise.js $@
    '';

    circles-directus =
      let
        dir_patched = pkgs.runCommand "dir_patched" { } ''
          mkdir $out
          cp -r ${workspaces.circles-directus}/* -t $out
          chmod -R +w $out
          cp -r ${pkgs.circles-pink-vendor.argon2}/* -t $out/libexec/circles-directus/node_modules/argon2
          cp -r ${pkgs.circles-pink-vendor.sharp}/* -t $out/libexec/circles-directus/node_modules/sharp
        '';
      in
      pkgs.writeShellScriptBin "directus" ''
         cd ${dir_patched}/libexec/circles-directus/node_modules/directus
        ./cli.js $@
      '';

    tasks-explorer-server =
      pkgs.writeShellScriptBin "tasks-explorer-server" ''
        cd ${workspaces.tasks-explorer-server}/libexec/tasks-explorer-server/node_modules/tasks-explorer-server
        ${pkgs.yarn}/bin/yarn start
      '';

    graphql-zeus = pkgs.writeShellScriptBin "zeus" ''
      ${workspaces.dev-utils}/libexec/dev-utils/node_modules/graphql-zeus/lib/CLI/index.js $@
    '';

    seed-db =
      pkgs.writeShellScriptBin "seed-db" ''
        cd ${workspaces.seed-db}/libexec/seed-db/node_modules/seed-db
        ${pkgs.yarn}/bin/yarn run ts-node src/index.ts
      '';
  };

  builds = {
    storybook = { serviceUrls }: pkgs.runCommand "storybook" { buildInputs = [ pkgs.yarn ]; } ''
      tmp=`mktemp -d`
      cp -r ${workspaces.storybook} $tmp/build
      chmod -R +w $tmp

      cp -r ${assets} $tmp/build/libexec/storybook/node_modules/assets/src
      cp -r ${pursOutput} $tmp/build/libexec/storybook/node_modules/generated/output
      cp -r ${zeus-client} $tmp/build/libexec/storybook/node_modules/@circles-pink/zeus-client/src

      export STORYBOOK_TASKS_EXPLORER_SERVER="${serviceUrls.tasks}"
      export STORYBOOK_DIRECTUS_URL="${serviceUrls.directus."/graphql"}"
      cd $tmp/build/libexec/storybook/node_modules/storybook
      OUTPUT_DIR=$out yarn build
    '';
  };

}
