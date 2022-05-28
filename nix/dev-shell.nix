{ pkgs, ... }:
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
    pkgs.dhall
    pkgs.dhall-lsp-server
    pkgs.graphviz
    pkgs.makefile2graph
    pkgs.depcruise
    pkgs.nixops
    pkgs.fish
    pkgs.graphql-zeus
    pkgs.signal-desktop
    pkgs.yarn2nix
    pkgs.jq
    #pkgs.virtualboxHeadless
    pkgs.notify-done
    pkgs.chokidar-cli
    pkgs.log-result
    pkgs.just
    pkgs.patch-json
    pkgs.gh
    pkgs.deadnix
    (pkgs.circles-pink.bumpNpmVersions (builtins.attrNames pkgs.circles-pink.ts.publicWorkspaces))
    #nodePackages.webpack
    pkgs.purs-tidy
    pkgs.purescript-docs-search
    pkgs.purescript-psa
    pkgs.dhall-json
    pkgs.spago2nix-extra.spago2nix-extra-cli
    (pkgs.writers.nodeRepl (builtins.attrValues {
      inherit (pkgs.nodePackages) fp-ts nijs glob yargs;
    }))
  ];

  # Change the prompt to show that you are in a devShell
  shellHook = ''
    . ${pkgs.complete-alias}/bin/complete_alias

    REPO_ROOT=$PWD
    export
    EDITOR=codium

    alias code=codium
    alias mk=make
    alias cd-root="cd $REPO_ROOT"

    complete -F _complete_alias mk
    complete -F _complete_alias code

  '' +
  "export PS1='\\e[1;36m@circles.pink\\e[0m:\\e[1;34m`echo $PWD | sed s#'$REPO_ROOT'#*#`\\e[0m$ '";
}
