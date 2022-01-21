{ pkgs, ... }:
let
  extensions = with pkgs.vscode-extensions; [
    jnoortheen.nix-ide
  ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [ ];

  vscodium-with-extensions = pkgs.vscode-with-extensions.override {
    vscode = pkgs.vscodium;
    vscodeExtensions = extensions;
  };
in
vscodium-with-extensions
