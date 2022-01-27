{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Circles Pink monorepo";
  inputs.nixpkgs.url = "nixpkgs";
  inputs.flake-compat = {
    url = github:edolstra/flake-compat;
    flake = false;
  };
  outputs = { self, nixpkgs, flake-compat }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [
          self.overlay
        ];
      });
    in
    {
      overlay = import ./nix/overlay.nix;

      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system}; in
        {
          hello = pkgs.writeText "hello.txt" "THIS!!!";
          sample = pkgs.circles-pink.yarn;
          test = let nixLib = pkgs.haskellPackages.yarn2nix.nixLib; in
            nixLib.buildNodePackage
              ({ src = nixLib.removePrefixes [ "node_modules" ] ./.; } //
              nixLib.callTemplate ./pkgs/ts/npm-package.nix
                (nixLib.buildNodeDeps (pkgs.callPackage ./pkgs/ts/npm-deps.nix { })));
          inherit pkgs;
        } // pkgs.circles-pink
      );

      checks = self.packages;

      devShell = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in
        pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            nixpkgs-fmt
            git
            vscode
            bashInteractive
            yarn
            nix-tree
            haskellPackages.yarn2nix
            miniserve
          ];

          # Change the prompt to show that you are in a devShell
          shellHook = "
            export PS1='\\e[1;32mnix@$PWD$ \\e[0m'
          " + ''
            alias code=codium
          '';
        });
    };
}
