{ pkgs, ... }:
let
  extensions = with pkgs.vscode-extensions; [
    jnoortheen.nix-ide
  ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "ide-purescript";
      publisher = "nwolverson";
      version = "0.25.8";
      sha256 = "sha256-z5/DO6XQSpHRzt0iDD6+CQ8ayFqYAw8lG5aigYPrN7A=";
    }

    {
      name = "language-purescript";
      publisher = "nwolverson";
      version = "0.2.7";
      sha256 = "sha256-cQnFWzqA4eBbjDiZGDFtg0/FUXf5weYOeZ1GiooYn9g=";
    }

    {
      name = "vscode-dhall-lsp-server";
      publisher = "dhall";
      version = "0.0.4";
      sha256 = "sha256-WopWzMCtiiLrx3pHNiDMZYFdjS359vu3T+6uI5A+Nv4=";
    }

    {
      name = "dhall-lang";
      publisher = "dhall";
      version = "0.0.4";
      sha256 = "sha256-7vYQ3To2hIismo9IQWRWwKsu4lXZUh0Or89WDLMmQGk=";
    }

    {
      name = "mdx";
      publisher = "silvenon";
      version = "0.1.0";
      sha256 = "sha256-4zdlvH91dTFKWBnHyjFEzBuT8g/dBzgNQpK2D/bD+tc=";
    }

    {
      name = "prettier-vscode";
      publisher = "esbenp";
      version = "9.3.0";
      sha256 = "sha256-hJgPjWf7a8+ltjmXTK8U/MwqgIZqBjmcCfHsAk2G3PA=";
    }
  ];

  vscodium-with-extensions = pkgs.vscode-with-extensions.override {
    vscode = pkgs.vscodium;
    vscodeExtensions = extensions;
  };
in
vscodium-with-extensions
