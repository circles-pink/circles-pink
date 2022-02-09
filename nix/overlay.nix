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
    };
})
