{ pkgs, ... }:
let
  inherit (builtins) map concatStringsSep attrValues;
  inherit (pkgs.lib) pipe;

  getGlob = pkg: ''".spago/${pkg.name}/${pkg.version}/src/**/*.purs"'';
  getGlobs = x: pipe x [ attrValues (map getGlob) (concatStringsSep " ") ];

  buildDependencies = name: spagoPkgs: pkgs.runCommand "${name}-purs-dependencies"
    {
      buildInputs = [
        spagoPkgs.buildSpagoStyle
        spagoPkgs.installSpagoStyle
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
    purs docs --compile-output $out/output --output $tmp ${getGlobs spagoPkgs.inputs}

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

  buildProject = name: { projectDir, spagoPkgs }: pkgs.runCommand "${name}-purs-project"
    {
      buildInputs = [
        spagoPkgs.buildSpagoStyle
      ];
      nativeBuildInputs = [
        pkgs.purescript
      ];
    }
    ''
      mkdir $out
      cd ${projectDir}
      cp --preserve=all -r ${projectDir}/output/* -t $out
      chmod -R +w $out
      build-spago-style "./src/**/*.purs" "./test/**/*.purs" --output $out
    '';

  testProject = name: { projectOut, spagoPkgs, nodeModules ? [ ] }: pkgs.runCommand "${name}-purs-project"
    {
      buildInputs = [
        spagoPkgs.buildSpagoStyle
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
        buildInputs = [ spagoPkgs.installSpagoStyle ];
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
        purs docs --output generated-docs/html ${getGlobs spagoPkgs.inputs} "src/**/*.purs"
        ${pkgs.purescript-docs-search}/bin/purescript-docs-search build-index
      '') [
    (x: pkgs.runCommand "${name}-purs-docs" { } "ln -s ${x}/generated-docs/html $out")
  ];

in
{
  inherit buildDependencies mkProjectDir buildProject testProject genDocs;
}
