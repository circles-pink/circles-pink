{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "Circles Pink monorepo";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: { });
      packages = forAllSystems (system: {
        hello =
          let
            pkgs = nixpkgsFor.${system}; in
          pkgs.writeText "hello.txt" "hello circles pink!";
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
            hello
          ];

          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
    };
}
