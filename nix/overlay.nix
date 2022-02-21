(final: prev: {
  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  cspell = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.cspell;

  ts-node = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.ts-node;

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
          node -e 'require("${purs.default}/CirclesPink.GenGraph").main()' $out
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

      assets = final.runCommand "assets" { } ''
        mkdir $out
        cp ${stateMachineGraphDot} $out/circles-state-machine.dot
        cp ${stateMachineGraphSvg} $out/circles-state-machine.svg
        cp ${makefileGraphSvg} $out/circles-makefile.svg
      '';

      publicDir = final.runCommand "output" { } ''
        cp -r ${ts.builds.storybook} $out
      '';
    };
})
