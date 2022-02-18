(final: prev: {
  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  cspell = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.cspell;

  ts-node = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.ts-node;

  circles-pink =
    rec {
      ts = (import ./pkgs/ts.nix {
        pkgs = final;
        pursOutput = purs.default;
      });

      patchTsTypes = final.writeShellScriptBin "patchTsTypes" ''
        cd ${ts.workspaces.dev-utils}/libexec/dev-utils/deps/dev-utils/
        ${final.ts-node}/bin/ts-node ./src/patchTsTypes.ts $@
      '';

      purs = (import ./pkgs/purs.nix { pkgs = final; });

      stateMachineGraph = final.runCommand "stateMachineGraph"
        { buildInputs = [ final.nodejs ]; }
        ''
          node -e 'require("${purs.default}/CirclesPink.GenGraph").main()' $out
        '';

      publicDir = final.runCommand "output" { } ''
        cp -r ${ts.builds.storybook} $out
        chmod -R +w $out
        cp -r ${final.runCommand "assets" {} ''mkdir $out''} $out/assets
      '';
    };
})
