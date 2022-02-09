{
  description = "Circles Pink monorepo";

  inputs.nixpkgs.url = "nixpkgs";

  inputs.purescript-tsd-gen.url = github:thought2/purescript-tsd-gen/flake;

  inputs.flake-utils.url = github:numtide/flake-utils;

  inputs.nix-filter.url = github:numtide/nix-filter;

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

        self = inputs.self;
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            overlay
            (prev: final: {
              purescript-tsd-gen = purescript-tsd-gen.defaultPackage.${system};
              spago2nix = easy-purescript-nix.spago2nix;
              nix-filter = nix-filter.lib;
            })
          ];
        };
        nix-filter = inputs.nix-filter;
        purescript-tsd-gen = inputs.purescript-tsd-gen;
        easy-purescript-nix = import inputs.easy-purescript-nix { inherit pkgs; };
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

        checks = self.packages;

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
              pkgs.nodePackages.typescript
              pkgs.nodejs
              pkgs.purescript
              pkgs.spago
              pkgs.purescript-tsd-gen
              pkgs.spago2nix
              pkgs.cspell
              pkgs.ts-node
              pkgs.circles-pink.patchTsTypes
              pkgs.gnumake
              pkgs.fff
              pkgs.nodePackages.prettier
            ];

            # Change the prompt to show that you are in a devShell
            shellHook = ''
              . ${pkgs.complete-alias}/bin/complete_alias
              
              REPO_ROOT=$PWD
              export EDITOR=codium

              alias code=codium
              alias mk=make
              alias cd-root="cd $REPO_ROOT"
              
              complete -F _complete_alias mk
              complete -F _complete_alias code

            '' +
            "export PS1='\\e[1;36m@circles.pink\\e[0m:\\e[1;34m`echo $PWD | sed s#'$REPO_ROOT'#*#`\\e[0m$ '";
          };
      });
}
