(final: prev: {
  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  circles-pink =
    {
      #ts = (import ./pkgs/ts.nix { pkgs = final; }).builds;

      ts = import ./../pkgs/ts/default.nix { pkgs = final; };

      cspell = (import ./pkgs/ts.nix { pkgs = final; }).bins.cspell;
    };
}
)
