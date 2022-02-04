{ pkgs, ... }: rec {
  foo = ../../.;


  #   workspaces = pkgs.yarn2nix-moretea.mkYarnWorkspace {
  #     src = ../../.;
  #   };

}
