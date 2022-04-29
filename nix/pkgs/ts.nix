{ pkgs, pursOutput, assets, zeus-client, ... }:
let

  inherit (pkgs) lib;
  inherit (lib) pipe;

in
rec {

  localPackages = builtins.mapAttrs (_: pkgs.lib.cleanSource)
    {
      "@circles-pink/content" = ../../pkgs/ts/${"@"}circles-pink/content;
      "@circles-pink/zeus-client" = ../../pkgs/ts/${"@"}circles-pink/zeus-client;
      "common" = ../../pkgs/ts/common;


      # dev-utils = ../../pkgs/ts/dev-utils;
      # common = ../../pkgs/ts/common;
      # storybook = ../../pkgs/ts/storybook;
      # cli = ../../pkgs/ts/cli;
      # circles-pink = ../../pkgs/ts/state-machine;
      # circles-directus = ../../pkgs/ts/circles-directus;
      # generated = ../../pkgs/ts/generated;
      # assets = ../../pkgs/ts/assets;
      # tasks-explorer = ../../pkgs/ts/tasks-explorer;
      # seed-db = ../../pkgs/ts/seed-db;
      # tasks-explorer-server = ../../pkgs/ts/tasks-explorer-server;
      # "@circles-pink/zeus-client" = ../../pkgs/ts/ts/circles-pink/zeus-client;
      # "@circles-pink/content" = ../../pkgs/ts/ts/circles-pink/content;
    };


  printPkgNameYarn2NixStyle = pn: pipe pn [
    (lib.splitString "/")
    (lib.intersperse "-")
    lib.concatStrings
    (lib.removePrefix "@")
  ];

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

  workspaces =
    let
      mapLocalPkg = name: source:
        let
          emptyWorkspace = emptyWorkspaces.${printPkgNameYarn2NixStyle name};
          dependencies = pipe [ ];
        in
        pkgs.runCommand name { } ''
          # Start with empty workspace
          cp -r ${emptyWorkspace} $out
          chmod -R +w $out

          # Add local package source
          cp -r ${source}/* -t $out/libexec/${name}/deps/${name} # WARNING: Hidden files omitted!
        '';
    in
    builtins.mapAttrs mapLocalPkg localPackages;


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

      cp -r ${assets} $tmp/build/libexec/storybook/node_modules/assets/src
      cp -r ${pursOutput} $tmp/build/libexec/storybook/node_modules/generated/output
      cp -r ${zeus-client} $tmp/build/libexec/storybook/node_modules/@circles-pink/zeus-client/src

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
      OUTPUT_DIR=$out yarn build

      chmod -R +w $tmp
      rm -rf $tmp
    '';
  };

}
