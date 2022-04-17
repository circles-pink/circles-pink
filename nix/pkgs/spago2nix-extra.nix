{ pkgs, ... }:
let
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
in
{
  inherit buildDependencies mkProjectDir buildProject testProject;
}
