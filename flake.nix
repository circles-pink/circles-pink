{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Circles Pink monorepo";

  inputs.nixpkgs.url = "nixpkgs";

  inputs.purescript-tsd-gen.url = github:thought2/purescript-tsd-gen/flake;

  inputs.flake-utils.url = github:numtide/flake-utils;

  inputs.easy-purescript-nix = {
    url = github:justinwoo/easy-purescript-nix;
    flake = false;
  };

  inputs.flake-compat = {
    url = github:edolstra/flake-compat;
    flake = false;
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlay = import ./nix/overlay.nix;

        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            overlay
            (prev: final: {
              purescript-tsd-gen = inputs.purescript-tsd-gen.defaultPackage.${system};
            })
          ];
        };
      in
      {

        packages =
          {
            hello = pkgs.writeText "hello.txt" "THIS!!!";
            sample = pkgs.circles-pink.yarn;

            foo = pkgs.purescript-tsd-gen;

            inherit pkgs;
          } // pkgs.circles-pink
        ;

        checks = inputs.self.packages;

        devShell =
          pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.nixpkgs-fmt
              pkgs.git
              pkgs.vscode
              pkgs.bashInteractive
              pkgs.yarn
              pkgs.nix-tree
              pkgs.miniserve
              pkgs.nodePackages.node2nix
              pkgs.nodePackages.purty
              pkgs.nodejs
              pkgs.purescript
              pkgs.spago
              pkgs.purescript-tsd-gen
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
