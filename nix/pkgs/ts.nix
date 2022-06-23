{ pkgs, pursOutput, assets, zeus-client, ... }:
let

  inherit (pkgs) lib runCommand;
  inherit (lib)
    pipe
    filterAttrs
    splitString
    intersperse
    concatMapStringsSep
    concatStrings
    removePrefix
    importJSON
    cleanSource
    concatMap
    mapAttrs';
  inherit (builtins) mapAttrs attrNames length elemAt;

in
rec {

  localPackages =
    {
      "@circles-pink/content" =
        cleanSource "${../../pkgs/ts}/@circles-pink/content";
      "@circles-pink/zeus-client" = pkgs.runCommand "" { } ''
        cp -r ${cleanSource ../../pkgs/ts/${"@"}circles-pink/zeus-client} $out
        chmod -R +w $out
        cp -r ${zeus-client} $out/src
      '';
      "@circles-pink/dependency-assistant" =
        cleanSource ../../pkgs/ts/${"@"}circles-pink/dependency-assistant;
      dev-utils =
        cleanSource ../../pkgs/ts/dev-utils;
      common =
        cleanSource ../../pkgs/ts/common;
      storybook =
        cleanSource ../../pkgs/ts/storybook;
      cli =
        cleanSource ../../pkgs/ts/cli;
      "@circles-pink/web-client" = cleanSource ../../pkgs/ts/${"@"}circles-pink/web-client;
      circles-directus =
        cleanSource ../../pkgs/ts/circles-directus;
      "@circles-pink/state-machine" =
        pkgs.runCommand "" { } ''
          cp -r ${cleanSource ../../pkgs/ts/${"@"}circles-pink/state-machine} $out
          chmod -R +w $out
          cp -r ${pursOutput} $out/output
        '';
      assets = pkgs.runCommand "" { } ''
        cp -r ${cleanSource ../../pkgs/ts/assets} $out
        chmod -R +w $out
        cp -r ${assets} $out/src
      '';
      tasks-explorer =
        cleanSource ../../pkgs/ts/tasks-explorer;
      seed-db =
        cleanSource ../../pkgs/ts/seed-db;
      tasks-explorer-server = cleanSource ../../pkgs/ts/tasks-explorer-server;
    };

  standalone = {
    "@circles-pink/purs-output-cleaner" = pkgs.yarn2nix-moretea.mkYarnPackage {
      src = cleanSource ../../pkgs/ts/${"@"}circles-pink/purs-output-cleaner;
    };
  };

  publicWorkspaces = {
    "@circles-pink/state-machine" = let name = "@circles-pink/state-machine"; in
      pipe
        workspaces.${name} [
        (x: runCommand name { } ''
          cp -r ${x}/libexec/${name}/deps/${name} $out
          chmod -R +w $out
          rm -rf $out/output/*/externs.cbor
        '')
      ];

    "@circles-pink/web-client" = let name = "@circles-pink/web-client"; in
      pipe
        workspaces.${name} [
        (x: runCommand name { } ''
          cp -r ${x} $out
          chmod -R +w $out
          cd $out/libexec/${name}/deps/${name}
          PATH=$PATH:../../../node_modules/.bin
          ${pkgs.yarn}/bin/yarn build
        '')
        (x: runCommand name { } ''
          ln -s ${x}/libexec/${name}/deps/${name} $out
        '')
      ];
  };

  printPkgNameYarn2NixStyle = pn: pipe pn [
    (splitString "/")
    (intersperse "-")
    concatStrings
    (removePrefix "@")
  ];

  validScope = "@circles-pink";

  parsePkgNameYarn2NixStyle = scope: pn:
    let scope' = "${removePrefix "@" scope}-"; in
    pipe pn [
      (splitString scope')
      (xs: if length xs == 1 then elemAt xs 0 else "${scope}/${elemAt xs 1}")
    ];

  src' = pkgs.nix-filter.filter {
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

  src = runCommand "src-cleaned" { } ''
    cp -r ${src'} $out
    cd $out
    chmod -R +w $out
    VERSION="1.0.0"
    ${concatMapStringsSep "\n" (w: "${pkgs.nodePackages.npm}/bin/npm version -w ${w} $VERSION") (attrNames publicWorkspaces)} \

    ${pkgs.nodePackages.npm}/bin/npm version --include-workspace-root --no-git-tag-version \
      "$VERSION"
  '';

  emptyWorkspaces = pipe
    (pkgs.yarn2nix-moretea.mkYarnWorkspace { inherit src; }) [
    (mapAttrs' (name: value: { name = parsePkgNameYarn2NixStyle validScope name; inherit value; }))
  ];

  fullWorkspaces = pkgs.yarn2nix-moretea.mkYarnWorkspace {
    src = pkgs.lib.cleanSource ../../.;
  };

  workspaces =
    let
      mapLocalPkg = name: source:
        let
          emptyWorkspace = emptyWorkspaces.${name};
          dependencies = pipe "${source}/package.json" [
            importJSON
            (_: _.dependencies)
            (filterAttrs (_: v: v == "*"))
            attrNames
          ];

          getDeps = dep: pipe "${workspaces.${dep}}/libexec/${dep}/deps/${dep}/package.json" [
            importJSON
            (_: _.dependencies)
            (filterAttrs (_: v: v == "*"))
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

    # dependency-assistant = pkgs.writeShellScriptBin "dependency-assistant" ''
    #   export PATH=${pkgs.nodejs}:$PATH
    #   ${workspaces.${"@circles-pink/dependency-assistant"}}/bin/circles-pink-dependency-assistant $@
    # '';

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
      cd $tmp/build/libexec/storybook/deps/storybook

      # node_modules need to be writeable, because storybook inevitably writes its cache in there!
      cp -r node_modules/ node_modules2
      rm -rf node_modules
      mv node_modules2 node_modules
      chmod -R +w node_modules

      node_modules/.bin/build-storybook --output-dir $out

      chmod -R +w $tmp
      rm -rf $tmp
    '';
  };

}


