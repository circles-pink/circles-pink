{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Circles Pink monorepo";

  inputs.nixpkgs.url = "nixpkgs";

  inputs.purescript-tsd-gen.url = github:thought2/purescript-tsd-gen/flake;

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.flake-compat = {
    url = github:edolstra/flake-compat;
    flake = false;
  };

  outputs = { self, nixpkgs, flake-compat, flake-utils, purescript-tsd-gen }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlay = import ./nix/overlay.nix;

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
          ];
        };
      in
      {

        packages =
          {
            hello = pkgs.writeText "hello.txt" "THIS!!!";
            sample = pkgs.circles-pink.yarn;

            inherit pkgs;
          } // pkgs.circles-pink
        ;

        checks = self.packages;

        devShell =
          pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              nixpkgs-fmt
              git
              vscode
              bashInteractive
              yarn
              nix-tree
              miniserve
              nodePackages.node2nix
              nodePackages.purty
              nodejs
              purescript
              spago
            ];

            # Change the prompt to show that you are in a devShell
            shellHook = "
            export PS1='\\e[1;32mnix@$PWD$ \\e[0m'
          " + ''
              alias code=codium
            '';
          };
      });
}
