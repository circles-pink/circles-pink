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
      inherit (builtins) listToAttrs filter concatMap;
      inherit (pkgs.lib) nameValuePair;

      mkLocalPkg = name: pkgs.stdenv.mkDerivation
        {
          inherit name;
          version = "local";
          src = attrs.${name}.location;
          phases = "installPhase";
          installPhase = "ln -s $src $out";
        };

      mkLocalPkgs' = deps: pipe
        deps [
        (filter (d: attrs ? ${d}))
        (concatMap (name:
          [ (nameValuePair name (mkLocalPkg name)) ] ++ (mkLocalPkgs' attrs.${name}.meta.dependencies)
        ))
      ];

      mkLocalPkgs = deps: pipe
        deps [
        mkLocalPkgs'
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
      inherit (pkgs) dhall-json spago2nix;
    in
    pkgs.writers.writeJSBin "spago2nix-extra"
      {
        libraries = builtins.attrValues { inherit (pkgs.nodePackages) fp-ts yargs glob nijs; };
      } ''
      const { jsToNix, NixImport, NixFile, NixFunInvocation, NixExpression } = require("nijs");
      const yargs = require('yargs/yargs')
      const glob = require("glob")
      const { hideBin } = require('yargs/helpers')
      const { writeFileSync, rmSync, renameSync } = require("fs")
      const { basename, join, relative } = require('path');
      const { execSync } = require('child_process')
      const { pipe, flow } = require('fp-ts/function')
      const R = require('fp-ts/Record')
      const S = require('fp-ts/string')
      const A = require('fp-ts/Array')

      const metaFile = "meta.json"
      const spagoPackagesFile = "spago-packages.nix"
      const indexFile = "default.nix"

      const parseOpts = () => yargs(hideBin(process.argv))
        .option('pkgs-dir', { type: 'string', required: true })
        .option('target-dir', { type: 'string', required: true })
        .parse()
    
      const nixImport = value => new NixImport(new NixFile({ value }))
      const nixFile = value => new NixFile({ value })

      const nixReadFile = paramExpr => new NixFunInvocation({ funExpr : new NixExpression("builtins.readFile"), paramExpr})
      const nixFromJSON = paramExpr => new NixFunInvocation({ funExpr : new NixExpression("builtins.fromJSON"), paramExpr})

      const handleDir = ({ targetDir, pkgsDir }) => (path) => {
        const dirName = basename(path);
        const { name, dependencies, sources } = pipe(
          execSync(`${dhall-json}/bin/dhall-to-json --file ''${path}/spago.dhall`),
          x => x.toString(),
          JSON.parse,
          R.filterWithIndex((k,_) => A.elem(S.Eq)(k)(["name", "dependencies", "sources"]))
        )
        
        if (!S.Eq.equals(dirName, name)) throw new Error(`''${dirName} does not equal ''${name}`)

        const targetDir_ = join(targetDir, name)
        execSync(`mkdir -p ''${targetDir_}`)

        execSync("${spago2nix}/bin/spago2nix generate", { cwd: path })
        renameSync(join(path, spagoPackagesFile), join(targetDir_, spagoPackagesFile))

        pipe(
          { name, dependencies, sources },
          x => JSON.stringify(x, null, 2),
          x => writeFileSync(join(targetDir_, metaFile), x)
        )

        const value = {
          spagoPkgs: pipe(join(name, spagoPackagesFile), x => "./" + x, nixImport),
          meta: pipe(join(name, metaFile), x => "./" + x, nixFile, nixReadFile, nixFromJSON),
          location: pipe(join(pkgsDir, name), x => relative(targetDir, x), nixFile)
        }

        return [name, value]
      }

      const main = () => {
        const opts = parseOpts()
        const { pkgsDir, targetDir } = opts 
        execSync(`mkdir -p ''${targetDir}`)
        execSync(`rm -rf ''${targetDir}/*`)
        const dirs = glob.sync(`''${pkgsDir}/*`)
        
        pipe(
          dirs,
          A.map(handleDir(opts)),
          R.fromEntries,
          x => jsToNix(x, true),
          x => writeFileSync(join(targetDir, indexFile), x)
        )
      }

      main()
    '';
in
{
  inherit buildMonorepo spago2nix-extra-cli;
}
