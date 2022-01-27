(final: prev: {
  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  circles-pink =
    {
      #ts = (import ./pkgs/ts.nix { pkgs = final; }).builds;

      cspell = (import ./pkgs/ts.nix { pkgs = final; }).bins.cspell;
    };
}
)
