(final: prev: {
  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  cspell = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.cspell;

  ts-node = (import ./pkgs/ts-root.nix { pkgs = final; }).bins.ts-node;

  circles-pink =
    {
      ts = (import ./pkgs/ts.nix { pkgs = final; }).default;

      ts-root = (import ./pkgs/ts-root.nix { pkgs = final; });

      patchTsTypes = final.writeShellScriptBin "patchTsTypes" ''
        ${final.ts-node}/bin/ts-node ${../scripts/ts/patchTsTypes.ts} $@
      '';

      purs = (import ./pkgs/purs.nix { pkgs = final; }).default;
    };
})
