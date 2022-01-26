(final: prev: {
  vscode = (import ./pkgs/vscode.nix { pkgs = final; });

  circles-pink = {
    yarn =
      import ./pkgs/yarn.nix { pkgs = final; };
  };

  cspell = (import ./pkgs/yarn.nix { pkgs = final; }).bins.cspell;
})
