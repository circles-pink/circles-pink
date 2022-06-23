(final: prev: rec {
  spago2nix-extra = import ./pkgs/spago2nix-extra.nix { pkgs = final; };

  chokidar-cli = circles-pink.yarn2nix.bins.chokidar-cli;

  purescript-docs-search = circles-pink.yarn2nix.bins.purescript-docs-search;

  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  cspell = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.cspell;

  ts-node = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.ts-node;

  depcruise = circles-pink.ts.bins.depcruise;

  directus = circles-pink.ts.bins.circles-directus;

  graphql-zeus = circles-pink.ts.bins.graphql-zeus;

  lib = prev.lib // (import ./pkgs/lib.nix { pkgs = final; });

  nix-fp-lite = import ./pkgs/nix-fp-lite.nix { lib = final.pkgs.lib; };

  fp = import ./pkgs/fp/default.nix { lib = final.pkgs.lib; };

  miraculix = import ./pkgs/miraculix.nix { pkgs = final; };

  nodePackages = prev.nodePackages // import ./pkgs/node2nix/default.nix { pkgs = final; };

  writers = prev.writers // {
    writeJSBin = name: { libraries ? [ ] }: content:
      let
        node-env = final.buildEnv {
          name = "node";
          paths = libraries;
          pathsToLink = [ "/lib/node_modules" ];
        };
      in
      final.writeShellScriptBin name ''
        export NODE_PATH=${node-env}/lib/node_modules
        ${final.nodejs}/bin/node ${final.writeText "${name}.js" content} $@
      '';

    nodeRepl = packages:
      let
        node-env = final.buildEnv {
          name = "node";
          paths = packages;
          pathsToLink = [ "/lib/node_modules" ];
        };
      in
      final.writeShellScriptBin "node-repl" ''
        export NODE_PATH=${node-env}/lib/node_modules
        ${final.nodejs}/bin/node $@
      '';

  };

  writeShellScriptBin' = name: { onPath ? [ ], env ? { } }: script:
    let
      exports = lib.mapAttrsToList (name: value: ''export ${name}="${value}"'') env;
    in
    prev.writeShellScriptBin name ''
      ${builtins.concatStringsSep "\n" exports}

      export PATH=${builtins.concatStringsSep ":" (map (p : "${p}/bin") onPath)}:$PATH

      ${script}
    '';


  patch-json = final.writeShellScriptBin "patch-json" (
    let
      mkCode = userJs: filePath: ''
        const f = ${userJs};
        const oldJson = JSON.parse(fs.readFileSync(\"${filePath}\"));
        const newJson = f(oldJson);
        fs.writeFileSync(\"${filePath}\", JSON.stringify(newJson + \"\n\", null, 2));
      '';
    in
    ''
      USER_JS_CODE="$1"
      FILE_PATH="$2"
      JS_CODE="${mkCode "$USER_JS_CODE" "$FILE_PATH"}"
      echo -e $JS_CODE
      
      ${final.pkgs.nodejs}/bin/node -e "$JS_CODE"
    ''
  );

  yarn2nix-to-node2nix = yarnPkg: final.runCommand "" { } ''
    mkdir -p $out/lib

    if ls ${yarnPkg}/libexec/*/*/node_modules/ 1> /dev/null 2>&1; then
      echo 1 > $out/b
      ln -s ${yarnPkg}/libexec/*/*/node_modules $out/lib/node_modules
    fi

    if ls ${yarnPkg}/libexec/*/node_modules/ 1> /dev/null 2>&1; then
      echo ${yarnPkg}/libexec/*/node_modules/ > $out/a
      ln -s ${yarnPkg}/libexec/*/node_modules $out/lib/node_modules
    fi
  '';

  runJS = name: opts: content:
    final.runCommand name { buildInputs = [ (final.writers.writeJS name opts content) ]; } name;

  yarnLockToJson = final.writeShellScriptBin "yarn-lock-to-json" ''
    cd ${circles-pink.ts.workspaces.dev-utils}/libexec/dev-utils/node_modules/dev-utils
    ${final.nodejs}/bin/node src/yarnLockToJson.js $@
  '';

  notify-done = final.writeShellScriptBin "notify-done" ''
    ${final.pkgs.bash}/bin/bash -c "$*"
    EXIT_CODE="$?"
    if [ $EXIT_CODE == 0 ]; then
      URGENCY=low;
    else
      URGENCY=critical;
    fi
    ${final.pkgs.notify-desktop}/bin/notify-desktop -t 5000 -u $URGENCY "$*" "$EXIT_CODE" > /dev/null
  '';

  log-result = final.writeShellScriptBin "log-result" ''
    UUID=`uuidgen`
    date
    echo START $UUID
    ${final.bash}/bin/bash -c "$*"
    EXIT_CODE="$?"
    CMD="$*"
    echo
    date
    echo FINISH $UUID
    echo $CMD
    if [ $EXIT_CODE == 0 ];
      then echo -e "\e[32mSUCCESS\e[0m";
      else echo -e "\e[31mFAILURE ($EXIT_CODE)\e[0m";
    fi
    echo
  '';

  circles-pink = import ./pkgs { pkgs = final; } //
    rec {

      bumpNpmVersions = workspaces:
        let
          inherit (final.lib) concatMapStringsSep;
          workspaces' = concatMapStringsSep " " (w: "-w ${w}") workspaces;
        in
        final.writeShellScriptBin "bump-npm-versions" ''
          RELEASE_TYPE="$1"
          ${final.nodePackages.npm}/bin/npm version ${workspaces'} "$RELEASE_TYPE"
          ${final.git}/bin/git add .
          ${final.nodePackages.npm}/bin/npm version --include-workspace-root --force "$RELEASE_TYPE"
        '';

      ts = (import ./pkgs/ts.nix {
        pkgs = final;
        pursOutput = purs.circles-pink-state-machine.default;
        inherit assets;
        inherit zeus-client;
      });

      ts2 = (import ./pkgs/ts2.nix {
        pkgs = final;
        pursOutput = purs.default;
        inherit assets;
        inherit zeus-client;
      });

      patchTsTypes = final.writeShellScriptBin "patchTsTypes" ''
        cd ${ts.workspaces.dev-utils}/libexec/dev-utils/deps/dev-utils/
        ${final.ts-node}/bin/ts-node ./src/patchTsTypes.ts $@
      '';

      purs = (import ./pkgs/purs.nix {
        pkgs = final;
        nodeModules = [
          "${ts.emptyWorkspaces."@circles-pink/state-machine"}/libexec/@circles-pink/state-machine/node_modules/@circles-pink/state-machine/node_modules"
          "${ts.emptyWorkspaces."@circles-pink/state-machine"}/libexec/@circles-pink/state-machine/node_modules"
        ];
      });

      stateMachineGraphDot = final.runCommand "stateMachineGraph"
        { buildInputs = [ final.nodejs ]; }
        ''
          node -e 'require("${purs.circles-pink-state-machine.default}/CirclesPink.Garden.GenGraph").main()' $out
        '';

      stateMachineGraphSvg = final.runCommand
        "stateMachineGraph"
        { buildInputs = [ final.graphviz ]; }
        ''
          dot -Tsvg ${stateMachineGraphDot} > $out
        '';

      makefileGraphSvg = final.runCommand
        "makefileGraphSvg"
        { buildInputs = [ final.graphviz final.makefile2graph final.gnumake ]; }
        ''
          make -Bnd -f ${../Makefile} | make2graph | dot -Tsvg -o $out
        '';

      moduleDependencyGraph = final.runCommand
        "moduleDependencyGraph"
        {
          buildInputs = [
            final.graphviz
            final.depcruise
            final.nodePackages.typescript
            final.nodejs
          ];
        }
        ''
          cd ${../pkgs/ts}
          depcruise --include-only '^.*/src' --output-type dot */src | dot -T svg > $out
        '';

      assets = final.runCommand
        "assets"
        { }
        ''
          mkdir $out
          cp ${stateMachineGraphDot} $out/circles-state-machine.dot
          cp ${stateMachineGraphSvg} $out/circles-state-machine.svg
          cp ${makefileGraphSvg} $out/circles-makefile.svg
          cp ${moduleDependencyGraph} $out/module-dep-graph.svg
          cp ${purs-moduleDependencyGraphSvg} $out/purs-moduleDependencyGraph.svg
        '';

      publicDir = { envVars }: final.runCommand "output" { } ''
        cp -r ${ts.builds.storybook {inherit envVars;}} $out
        chmod -R +w $out
        ln -s ${purs.circles-pink-state-machine.docs} $out/purs-docs
      '';

      runGarden = { envVars }: final.writeShellScriptBin "run-garden" ''
        export NODE_PATH=${ts.workspaces."@circles-pink/state-machine"}/libexec/@circles-pink/state-machine/node_modules:${ts.workspaces."@circles-pink/state-machine"}/libexec/@circles-pink/state-machine/deps/@circles-pink/state-machine/node_modules
        export GARDEN_API=${envVars.gardenApi}
        export GARDEN_API_USERS=${envVars.gardenApiUsers}
        export GARDEN_GRAPH_API=${envVars.gardenGraphApi}
        export GARDEN_SUBGRAPH_NAME="${envVars.gardenSubgraphName}"
        export GARDEN_RELAY="${envVars.gardenRelay}"
        export GARDEN_HUB_ADDRESS="${envVars.gardenHubAddress}"
        export GARDEN_PROXY_FACTORY_ADRESS="${envVars.gardenProxyFactoryAddress}"
        export GARDEN_SAFE_MASTER_ADDRESS="${envVars.gardenSafeMasterAddress}"
        export GARDEN_ETHEREUM_NODE_WS="${envVars.gardenEthereumNodeWebSocket}"
        ${final.nodejs}/bin/node -e 'require("${purs.default}/CirclesPink.Garden.ApiScript").main()' $@
      '';

      purs-deps-json =
        let
          inherit (purs.circles-pink-state-machine) sources dependencies;
        in
        final.runCommand
          "purs-deps.json"
          { buildInputs = [ final.purescript final.jq ]; }
          ''
            shopt -s globstar
            purs graph  ${sources}/**/*.purs ${dependencies}/.spago/*/*/src/**/*.purs | jq > $out
          '';

      purs-deps = final.runCommand
        "purs-deps"
        { }
        (
          let
            src = final.writeText "purs-deps" ''
              #!${final.nodejs}/bin/node
              require("${purs.circles-pink-state-machine.output}/PursDeps").main()
            '';
          in
          ''
            mkdir -p $out/bin
            cp ${src} $out/bin/purs-deps
            chmod +x $out/bin/purs-deps
          ''
        );

      purs-moduleDependencyGraphDot = final.runCommand
        "purs-moduleDependencyGraph.dot"
        { buildInputs = [ purs-deps ]; }
        ''
          purs-deps --depsJsonPath ${purs-deps-json} > $out
        '';

      purs-moduleDependencyGraphSvg = final.runCommand
        "purs-moduleDependencyGraph.svg"
        { buildInputs = [ final.graphviz ]; }
        ''
          dot -Tsvg ${purs-moduleDependencyGraphDot} > $out
        '';

      circles-directus = ts.bins.circles-directus;

      seed-db = ts.bins.seed-db;

      tasks-explorer-server = ts.bins.tasks-explorer-server;

      zeus-client = final.runCommand
        "zeus-client"
        {
          buildInputs = [ final.graphql-zeus ];
        }
        ''
          mkdir $out
          zeus ${../materialized/directus-dump/directus-api-admin.graphql} $out/admin --node
          zeus ${../materialized/directus-dump/directus-api-public.graphql} $out/public
        '';

      purs-output-cleaner = ts.standalone."@circles-pink/purs-output-cleaner";
    };
})
