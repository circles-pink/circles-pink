{ pkgs, ... }:
let
  inherit (builtins) map concatStringsSep attrValues;
  inherit (pkgs.lib) pipe mapAttrs;

  cpPackage = pkg:
    let
      target = ".spago/${pkg.name}/${pkg.version}";
    in
    ''
      if [ ! -e ${target} ]; then
        echo "Installing ${target}."
        mkdir -p ${target}
        cp --no-preserve=mode,ownership,timestamp -r ${toString pkg.outPath}/* ${target}
      else
        echo "${target} already exists. Skipping."
      fi
    '';

  installSpagoStyle = spagoPkgs: pkgs.writeShellScriptBin "install-spago-style" ''
    set -e
    echo installing dependencies...
    ${builtins.toString (builtins.map cpPackage (builtins.attrValues spagoPkgs))}
    echo "echo done."
  '';

  buildSpagoStyle = spagoPkgs: pkgs.writeShellScriptBin "build-spago-style" ''
    set -e
    echo building project...
    purs compile ${builtins.toString (builtins.map getGlob (builtins.attrValues spagoPkgs))} "$@"
    echo done.
  '';

  getGlob = pkg: ''".spago/${pkg.name}/${pkg.version}/src/**/*.purs"'';

  getGlobs = x: pipe x [ attrValues (map getGlob) (concatStringsSep " ") ];

  buildDependencies = name: spagoPkgs: pkgs.runCommand "${name}-purs-dependencies"
    {
      buildInputs = [
        (buildSpagoStyle spagoPkgs)
        (installSpagoStyle spagoPkgs)
      ];
      nativeBuildInputs = [
        pkgs.purescript
        pkgs.spago
      ];
    } ''
    mkdir $out    
    cd $out
    install-spago-style
    build-spago-style

    # Compile docs
    chmod -R +w $out
    tmp=`mktemp -d`
    purs docs --compile-output $out/output --output $tmp ${getGlobs spagoPkgs}

    REGEX_DATE='[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}T[0-9]\{2\}:[0-9]\{2\}:[0-9]\{2\}.[0-9]*Z'
    NEW_DATE='1970-01-01T00:00:00.000Z'
    sed -i "s/$REGEX_DATE/$NEW_DATE/g" $out/output/cache-db.json
  '';

  mkProjectDir = name: { dependencies, sources, testSources }: pkgs.runCommand "${name}-purs-project"
    { }
    ''
      mkdir $out
      ln -s ${dependencies}/.spago $out/.spago
      ln -s ${dependencies}/output $out/output
      ln -s ${pkgs.lib.cleanSource sources} $out/src
      ln -s ${pkgs.lib.cleanSource testSources} $out/test
    '';

  buildProject =
    let
      s = pkgs.fp.string;
    in
    name: { projectDir, spagoPkgs, censorCodes ? [ ] }: pkgs.runCommand "${name}-purs-project"
      { buildInputs = [ pkgs.purescript-psa pkgs.purescript ]; }
      ''
        mkdir $out
        cd ${projectDir}
        cp --preserve=all -r ${projectDir}/output/* -t $out
        chmod -R +w $out
        
        psa \
          --strict --censor-lib --censor-codes=${s.joinWith "," censorCodes} \
          compile \
          ${getGlobs spagoPkgs} "./src/**/*.purs" "./test/**/*.purs" --output $out
      '';

  testProject = name: { projectOut, spagoPkgs, nodeModules ? [ ] }: pkgs.runCommand "${name}-purs-project"
    {
      buildInputs = [
        (buildSpagoStyle spagoPkgs)
      ];
      nativeBuildInputs = [
        pkgs.purescript
        pkgs.nodejs
      ];
    }
    ''
      export NODE_PATH=${builtins.concatStringsSep ":" nodeModules}
      node -e 'require("${projectOut}/Test.Main").main()' > $out
    '';

  genDocs = name: { projectOut, spagoPkgs, projectDir }: pipe
    (pkgs.runCommand "${name}-purs-docs"
      {
        buildInputs = [ (installSpagoStyle spagoPkgs) ];
        nativeBuildInputs = [ pkgs.purescript ];
      }
      ''
        mkdir $out
        cp --preserve=all -r ${projectDir}/.spago/ $out/.spago
        cp --preserve=all -r ${projectDir}/output/ $out/output
        cp --preserve=all -r ${projectDir}/src/ $out/src
        cp --preserve=all -r ${projectDir}/test/ $out/test
        
        cd $out
        chmod -R +w $out/output
        purs docs --output generated-docs/html ${getGlobs spagoPkgs} "src/**/*.purs"
        ${pkgs.purescript-docs-search}/bin/purescript-docs-search build-index \
          --package-name ${name}
      '') [
    (x: pkgs.runCommand "${name}-purs-docs" { } "ln -s ${x}/generated-docs/html $out")
  ];

  build = { name, location, spagoPkgs, censorCodes ? [ ], nodeModules }:
    let
      sources = "${location}/src";
      testSources = "${location}/test";
      dependencies = buildDependencies name spagoPkgs;
      projectDir = mkProjectDir name { inherit dependencies sources testSources; };
      projectOut = buildProject name { inherit projectDir spagoPkgs censorCodes; };
      projectTests = testProject name { inherit projectOut spagoPkgs nodeModules; };
      docs = genDocs name { inherit projectOut spagoPkgs projectDir; };
    in
    {
      inherit docs sources dependencies;
      output = projectOut;
      tests = projectTests;
    };

  buildMonorepo = attrs:
    let
      inherit (builtins) listToAttrs filter;
      inherit (pkgs.lib) nameValuePair;

      mkLocalPkgs = deps: pipe
        deps [
        (filter (d: attrs ? ${d}))
        (map (name: nameValuePair name (pkgs.stdenv.mkDerivation {
          inherit name;
          version = "local";
          src = attrs.${name}.location;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        })))
        listToAttrs
      ];
    in
    pipe
      attrs [
      (mapAttrs (_: { spagoPkgs, meta, location, censorCodes ? [ ], nodeModules ? [ ] }:
        build
          {
            name = meta.name;
            spagoPkgs = (spagoPkgs { inherit pkgs; }).inputs // mkLocalPkgs meta.dependencies;
            inherit censorCodes nodeModules location;
          }))
    ];


  spago2nix-extra-cli =
    let
      inherit (pkgs)
        spago2nix
        jq
        dhall-json;
    in
    pkgs.writeShellScriptBin
      "spago2nix-extra"
      ''
        PKGS_DIR=$1
        TARGET_DIR=$2
        REL=$3
        rm -rf $TARGET_DIR
        mkdir -p $TARGET_DIR
        INDEX_FILE=""

        for d in $PKGS_DIR/*/ ; do
          PKG_NAME=`basename $d`
          TARGET="$TARGET_DIR/$PKG_NAME"
          mkdir -p "$TARGET"
          ${spago2nix}/bin/spago2nix generate
          mv spago-packages.nix -t "$TARGET"
          # TODO: Add check that folder name equals name in spago.dhall
          ${dhall-json}/bin/dhall-to-json --file $d/spago.dhall \
            | ${jq}/bin/jq '{name, dependencies, sources}' \
            > "$TARGET/meta.json"
          INDEX_FILE="$INDEX_FILE""  $PKG_NAME = { spagoPkgs = import ./$PKG_NAME/spago-packages.nix; meta = readJson ./$PKG_NAME/meta.json; location = ""$REL/$d".";}; \n"
        done

        LET_IN="let inherit (builtins) fromJSON readFile; readJson = x: fromJSON (readFile x); in"
        echo -e "$LET_IN\n{\n$INDEX_FILE}" > "$TARGET_DIR/default.nix"
      '';
in
{
  inherit buildMonorepo spago2nix-extra-cli;
}
