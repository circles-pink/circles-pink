{ pkgs, pursOutput, assets, zeus-client, ... }:
let

  inherit (pkgs) lib;
  inherit (lib)
    pipe filterAttrs splitString intersperse concatMapStringsSep
    concatStrings removePrefix importJSON cleanSource concatMap;
  inherit (builtins) mapAttrs attrNames readFile;


in
rec {

  localPackages' = mapAttrs (_: pkgs.lib.cleanSource)
    {
      "@circles-pink/content" = ../../pkgs/ts/${"@"}circles-pink/content;
      "@circles-pink/zeus-client" = ../../pkgs/ts/${"@"}circles-pink/zeus-client;
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
    };

  # cp -r ${assets} $tmp/build/libexec/storybook/node_modules/assets/src
  # cp -r ${pursOutput} $tmp/build/libexec/storybook/node_modules/generated/output
  # cp -r ${zeus-client} $tmp/build/libexec/storybook/node_modules/@circles-pink/zeus-client/src

  localPackages =
    {
      "@circles-pink/content" = cleanSource ../../pkgs/ts/${"@"}circles-pink/content;
      "@circles-pink/zeus-client" = pkgs.runCommand "" { } ''
        cp -r ${cleanSource ../../pkgs/ts/${"@"}circles-pink/zeus-client} $out
        chmod -R +w $out
        cp -r ${zeus-client} $out/src
      '';
      dev-utils = cleanSource ../../pkgs/ts/dev-utils;
      common = cleanSource ../../pkgs/ts/common;
      storybook = cleanSource ../../pkgs/ts/storybook;
      cli = cleanSource ../../pkgs/ts/cli;
      circles = cleanSource ../../pkgs/ts/circles;
      circles-directus = cleanSource ../../pkgs/ts/circles-directus;
      generated = pkgs.runCommand "" { } ''
        cp -r ${cleanSource ../../pkgs/ts/generated} $out
        chmod -R +w $out
        cp -r ${pursOutput} $out/output
      '';
      assets = pkgs.runCommand "" { } ''
        cp -r ${cleanSource ../../pkgs/ts/assets} $out
        chmod -R +w $out
        cp -r ${assets} $out/src
      '';
      tasks-explorer = cleanSource ../../pkgs/ts/tasks-explorer;
      seed-db = cleanSource ../../pkgs/ts/seed-db;
      tasks-explorer-server = cleanSource ../../pkgs/ts/tasks-explorer-server;
    };

  printPkgNameYarn2NixStyle = pn: pipe pn [
    (splitString "/")
    (intersperse "-")
    concatStrings
    (removePrefix "@")
  ];

  src = pkgs.nix-filter.filter {
    root = pkgs.lib.cleanSource ../../.;
    include = [
      "package.json"
      "yarn.lock"
    ] ++ (
      pkgs.lib.pipe localPackages [
        attrNames
        (map (name: "pkgs/ts/${name}/package.json"))
      ]
    )
    ;
  };

  emptyWorkspaces = pkgs.yarn2nix-moretea.mkYarnWorkspace {
    inherit src;
  };

  fullWorkspaces = pkgs.yarn2nix-moretea.mkYarnWorkspace {
    src = pkgs.lib.cleanSource ../../.;
  };

  workspaces =
    let
      mapLocalPkg = name: source:
        let
          emptyWorkspace = emptyWorkspaces.${printPkgNameYarn2NixStyle name};
          dependencies = pipe "${source}/package.json" [
            importJSON
            (_: _.dependencies)
            (filterAttrs (k: v: v == "*"))
            attrNames
          ];

          getDeps = dep: pipe "${workspaces.${dep}}/libexec/${dep}/deps/${dep}/package.json" [
            importJSON
            (_: _.dependencies)
            (filterAttrs (k: v: v == "*"))
            attrNames
          ];

          transitiveDependencies = pipe dependencies [
            (concatMap (dep: [ dep ] ++ getDeps dep))
          ];

          mkDep = dep: ''
            chmod -R +w $out
            # rm -rf $out/libexec/${name}/deps/${dep}
            # ln -sf ${workspaces.${dep}}/libexec/${dep}/deps/${dep} $out/libexec/${name}/deps/${dep}

            #rm -rf $out/libexec/${name}/deps/${dep}
            echo "copy ${dep}"
            cp -r ${workspaces.${dep}}/libexec/${dep}/deps/${dep}/. $out/libexec/${name}/deps/${dep}
          '';
        in
        pkgs.runCommand name { } ''
          # Start with empty workspace
          cp -r ${emptyWorkspace} $out
          chmod -R +w $out

          # Add local package source
          rm -rf $out/libexec/${name}/deps/${name}
          mkdir $out/libexec/${name}/deps/${name}
          cp -r ${source}/. $out/libexec/${name}/deps/${name}
        
          # Link Dependencies
          ${concatMapStringsSep "\n" mkDep transitiveDependencies}
        '';
    in
    mapAttrs mapLocalPkg localPackages;


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
    storybook = { envVars }: pkgs.runCommand "storybook" { buildInputs = [ pkgs.yarn ]; } ''
      tmp=`mktemp -d`
      cp -r ${workspaces.storybook} $tmp/build
      chmod -R +w $tmp

      export STORYBOOK_TASKS_EXPLORER_SERVER="${envVars.tasks}"
      export STORYBOOK_DIRECTUS_URL="${envVars.directus."/graphql"}"
      export STORYBOOK_GARDEN_API="${envVars.gardenApi}"
      export STORYBOOK_GARDEN_API_USERS="${envVars.gardenApiUsers}"
      export STORYBOOK_GARDEN_GRAPH_API="${envVars.gardenGraphApi}"
      export STORYBOOK_GARDEN_SUBGRAPH_NAME="${envVars.gardenSubgraphName}"
      export STORYBOOK_GARDEN_RELAY="${envVars.gardenRelay}"
      export STORYBOOK_GARDEN_HUB_ADDRESS="${envVars.gardenHubAddress}"
      export STORYBOOK_GARDEN_PROXY_FACTORY_ADRESS="${envVars.gardenProxyFactoryAddress}"
      export STORYBOOK_GARDEN_SAFE_MASTER_ADDRESS="${envVars.gardenSafeMasterAddress}"
      export STORYBOOK_GARDEN_ETHEREUM_NODE_WS="${envVars.gardenEthereumNodeWebSocket}"
      cd $tmp/build/libexec/storybook/node_modules/storybook

      ../../node_modules/.bin/build-storybook --output-dir $out

      chmod -R +w $tmp
      rm -rf $tmp
    '';
  };

}


