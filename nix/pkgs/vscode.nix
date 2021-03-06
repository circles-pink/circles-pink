{ pkgs, ... }:
let
  extensions = with pkgs.vscode-extensions; [
    jnoortheen.nix-ide
  ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "ide-purescript";
      publisher = "nwolverson";
      version = "0.25.12";
      sha256 = "sha256-tgZ0PnWrSDBNKBB5bKH/Fmq6UVNSRYZ8HJdzFDgxILk=";
    }

    {
      name = "language-purescript";
      publisher = "nwolverson";
      version = "0.2.8";
      sha256 = "sha256-2uOwCHvnlQQM8s8n7dtvIaMgpW8ROeoUraM02rncH9o=";
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

    {
      name = "errorlens";
      publisher = "usernamehw";
      version = "3.4.1";
      sha256 = "sha256-cJ1/jfCU+Agiyi1Qdd0AfyOTzwxOEfox4vLSJ0/UKNc=";
    }

    {
      name = "glassit";
      publisher = "s-nlf-fh";
      version = "0.2.4";
      sha256 = "sha256-YmohKiypAl9sbnmg3JKtvcGnyNnmHvLKK1ifl4SmyQY=";
    }

    {
      name = "just";
      publisher = "skellock";
      version = "2.0.0";
      sha256 = "sha256-FOp/dcW0+07rADEpUMzx+SGYjhvE4IhcCOqUQ38yCN4=";
    }

  ];

  vscodium-with-extensions = pkgs.vscode-with-extensions.override {
    vscode = pkgs.vscodium;
    vscodeExtensions = extensions;
  };
in
vscodium-with-extensions
