{
  description = "Circles Pink monorepo";

  inputs.nixpkgs.url = "nixpkgs";

  inputs.purescript-tsd-gen.url = github:thought2/purescript-tsd-gen/flake;

  inputs.flake-utils.url = github:numtide/flake-utils;

  inputs.nix-filter.url = github:numtide/nix-filter;

  inputs.nixops.url = github:NixOS/nixops;

  inputs.circles-pink-vendor.url = github:circles-pink/circles-pink-vendor;

  inputs.easy-purescript-nix = {
    url = github:justinwoo/easy-purescript-nix;
    flake = false;
  };

  inputs.flake-compat = {
    url = github:edolstra/flake-compat;
    flake = false;
  };

  inputs.hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";

  inputs.flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";

  inputs.deadnix.url = "github:astro/deadnix";

  inputs.circles-toolbelt = {
    url = "github:CirclesUBI/circles-toolbelt";
    flake = false;
  };

  inputs.circles-docker = {
    url = "github:CirclesUBI/circles-docker";
    flake = false;
  };

  outputs = inputs:
    let
      devSystems = [ "x86_64-linux" "aarch64-darwin" ];
      ciSystems = devSystems;
      cdSystem = "x86_64-linux";
      self = inputs.self;

      system = "x86_64-linux";

      overlays = [
        overlay
        (prev: final: {
          purescript-tsd-gen = purescript-tsd-gen.defaultPackage.${system};
          inherit (easy-purescript-nix) purs-tidy spago2nix;
          nix-filter = nix-filter.lib;
          nixopsLatest = inputs.nixops.defaultPackage.${system};
          circles-pink-vendor = inputs.circles-pink-vendor.packages.${system};
        })
        inputs.deadnix.overlay
      ];

      pkgs = import inputs.nixpkgs {
        inherit system;
        inherit overlays;
      };
      nix-filter = inputs.nix-filter;
      purescript-tsd-gen = inputs.purescript-tsd-gen;
      easy-purescript-nix = import inputs.easy-purescript-nix { inherit pkgs; };
      overlay = import ./nix/overlay.nix;
      checks = import ./nix/checks.nix { inherit pkgs; };

      perSystem = (inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ]
        (system:
          {
            # inherit overlays;

            legacyPackages = { inherit pkgs; } // pkgs.circles-pink;

            packages = {
              ci =
                let
                  inherit (pkgs.lib) mapAttrsToList pipe;
                  inherit (builtins) concatStringsSep replaceStrings;
                in
                pkgs.runCommand "ci" { } ''
                  mkdir $out
                  ${pipe self.checks.${system} [(mapAttrsToList (k: v: "ln -s ${v} $out/${replaceStrings ["/"] ["--"] k}")) (concatStringsSep "\n")]}
                '';

              checkouts = pkgs.runCommand "checkouts" { } ''
                mkdir $out
                cp -R ${inputs.circles-docker} $out/circles-docker
                chmod -R +w $out
                mv $out/circles-docker/.env.example $out/circles-docker/.env

                cp -R ${inputs.circles-toolbelt} $out/circles-toolbelt
              '';
            };

            checks = self.packages; # gets overwritten below. todo: change!

            devShells.default = import ./nix/dev-shell.nix { inherit pkgs; };

            devShells.garden =
              pkgs.mkShell {
                nativeBuildInputs = [
                  pkgs.nodejs-12_x
                  pkgs.docker-compose
                ];

                # Change the prompt to show that you are in a devShell
                shellHook = "";
              };


          }));

      general =
        {

          checks = {
            x86_64-linux =
              let
                effects = self.effects { src.ref = null; };
              in
              {
                deploy-nixops-example-prebuilt = effects.deploy.prebuilt;
                #deploy-nixops-example-dependencies = effects.nixops-example.dependencies;
                #inherit (effects) publish;
              } // checks;
          };

          ciNix = args@{ src }: inputs.flake-compat-ci.lib.recurseIntoFlakeWith {
            flake = self;
            systems = ciSystems;
            effectsArgs = args;
          };

          nixopsConfigurations.default =
            let
              accessKeyId = "nixops-example";
              region = "us-east-1";
              tags = { };
              name = "default";
            in
            {
              nixpkgs = inputs.nixpkgs;
              network.description = name;

              network = {
                storage.memory = { };
              };

            }
            // (import ./networks/prod.nix { inherit pkgs; });

          effects = { src }:
            let
              nixpkgs = inputs.nixpkgs.legacyPackages.${cdSystem};
              effects = inputs.hercules-ci-effects.lib.withPkgs pkgs;
            in
            {
              deploy = effects.runIf (src.ref == "refs/heads/main")
                (effects.runNixOps2
                  {
                    flake = self;

                    userSetupScript = ''
                      writeSSHKey
                      ssh -o "StrictHostKeyChecking no" root@circles.pink whoami
                      mkdir -p ~/.config/nix
                      echo 'experimental-features = nix-command flakes' >>~/.config/nix/nix.conf
                      readSecretJSON secrets . > /secrets.json
                    '';

                    secretsMap = {
                      "ssh" = "default-ssh";
                      "secrets" = "secrets";
                    };
                    forgetState = true;

                  });
              publish = effects.runIf (src.ref == "refs/heads/main")
                (effects.mkEffect {
                  userSetupScript = ''
                    NODE_AUTH_TOKEN=`readSecretString secrets '."npm-token"'`;
                    ${pkgs.nodePackages.npm}/bin/npm config set "//registry.npmjs.org/:_authToken" "$NODE_AUTH_TOKEN"
                  '';

                  effectScript =
                    let
                      inherit (builtins) attrValues;
                      inherit (pkgs.lib) concatMapStringsSep;
                      packageJsonUrl = "https://raw.githubusercontent.com/circles-pink/circles-pink/main/package.json";
                      publish = pkgs.writeShellScriptBin "publish" ''
                        DIR="$1"
                          ${pkgs.nodePackages.npm}/bin/npm publish --verbose --access public $DIR/
                      '';
                      inherit (pkgs.circles-pink.ts) publicWorkspaces;
                    in
                    ''
                      NEW_VERSION=`cat ${./package.json} | ${pkgs.jq}/bin/jq '.version'`
                      CURRENT_VERSION=`${pkgs.curl}/bin/curl https://registry.npmjs.org/@circles-pink/web-client | ${pkgs.jq}/bin/jq '."dist-tags".latest'`

                      if [ "$CURRENT_VERSION" == "$NEW_VERSION" ]
                      then
                      echo "Nothing to release."
                      exit 0
                      fi

                      ${concatMapStringsSep "\n" (ws: "${publish}/bin/publish ${ws}") (attrValues publicWorkspaces)}
                    '';

                  secretsMap = {
                    "secrets" = "secrets";
                  };
                });
            };
        };

    in
    perSystem // general;
}

