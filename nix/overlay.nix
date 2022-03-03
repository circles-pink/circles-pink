(final: prev: rec {
  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  cspell = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.cspell;

  ts-node = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.ts-node;

  depcruise = circles-pink.ts.bins.depcruise;

  circles-pink =
    rec {
      ts = (import ./pkgs/ts.nix {
        pkgs = final;
        pursOutput = purs.default;
        inherit assets;
      });

      patchTsTypes = final.writeShellScriptBin "patchTsTypes" ''
        cd ${ts.workspaces.dev-utils}/libexec/dev-utils/deps/dev-utils/
        ${final.ts-node}/bin/ts-node ./src/patchTsTypes.ts $@
      '';

      purs = (import ./pkgs/purs.nix { pkgs = final; });

      stateMachineGraphDot = final.runCommand "stateMachineGraph"
        { buildInputs = [ final.nodejs ]; }
        ''
          node -e 'require("${purs.default}/CirclesPink.Garden.GenGraph").main()' $out
        '';

      stateMachineGraphSvg = final.runCommand "stateMachineGraph"
        { buildInputs = [ final.graphviz ]; }
        ''
          dot -Tsvg ${stateMachineGraphDot} > $out
        '';

      makefileGraphSvg = final.runCommand "makefileGraphSvg"
        { buildInputs = [ final.graphviz final.makefile2graph final.gnumake ]; }
        ''
          make -Bnd -f ${../Makefile} | make2graph | dot -Tsvg -o $out
        '';

      moduleDependencyGraph = final.runCommand "moduleDependencyGraph"
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

      assets = final.runCommand "assets" { } ''
        mkdir $out
        cp ${stateMachineGraphDot} $out/circles-state-machine.dot
        cp ${stateMachineGraphSvg} $out/circles-state-machine.svg
        cp ${makefileGraphSvg} $out/circles-makefile.svg
        cp ${moduleDependencyGraph} $out/module-dep-graph.svg
      '';

      publicDir = final.runCommand "output" { } ''
        cp -r ${ts.builds.storybook} $out
      '';

      runGarden = final.writeShellScriptBin "run-garden" ''
        export NODE_PATH=${ts.workspaces.generated}/libexec/generated/node_modules
        ${final.nodejs}/bin/node -e 'require("${purs.default}/CirclesPink.Garden.ApiScript").main()' $@
      '';
    };
})
