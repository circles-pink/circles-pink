{ pkgs, ... }: {

  pursTests = pkgs.circles-pink.purs.projectTests;

  nixLint =
    let
      # This is only the beginning, in the end all nix files should be linted
      paths = [
        ./pkgs/ts.nix
        ./checks.nix
      ];
    in
    pkgs.runCommand "nix-lint" { } ''
      ${pkgs.deadnix}/bin/deadnix --fail ${builtins.concatStringsSep " " paths} > $out
    '';
}
