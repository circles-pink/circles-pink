(final: prev: {
  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  circles-pink =
    {
      ts = (import ./pkgs/ts.nix { pkgs = final; });

      purs = (import ./pkgs/purs.nix { pkgs = final; });

      #ts = import ./../pkgs/ts/default.nix { pkgs = final; };

      cspell = (import ./pkgs/ts.nix { pkgs = final; }).bins.cspell;
    };
}
)
