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
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = import ./nix/overlay.nix;

      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system}; in
        {
          hello = pkgs.writeText "hello.txt" "hello circles pink! #0";
          sample = pkgs.circles-pink.yarn;
          inherit pkgs;
        });

      defaultPackage = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system}; in
        pkgs.writeText "hello.txt" "hello circles pink!");

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
