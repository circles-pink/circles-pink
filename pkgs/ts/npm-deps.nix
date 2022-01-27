{ fetchurl, fetchgit }:
  self:
    super:
      let
        registries = {
          yarn = n:
            v:
              "https://registry.yarnpkg.com/${n}/-/${n}-${v}.tgz";
          npm = n:
            v:
              "https://registry.npmjs.org/${n}/-/${n}-${v}.tgz";
          };
        nodeFilePackage = key:
          version:
            registry:
              sha1:
                deps:
                  super._buildNodePackage {
                    inherit key version;
                    src = fetchurl {
                      url = registry key version;
                      inherit sha1;
                      };
                    nodeBuildInputs = deps;
                    };
        nodeFileLocalPackage = key:
          version:
            path:
              sha1:
                deps:
                  super._buildNodePackage {
                    inherit key version;
                    src = builtins.path { inherit path; };
                    nodeBuildInputs = deps;
                    };
        nodeGitPackage = key:
          version:
            url:
              rev:
                sha256:
                  deps:
                    super._buildNodePackage {
                      inherit key version;
                      src = fetchgit { inherit url rev sha256; };
                      nodeBuildInputs = deps;
                      };
        identityRegistry = url:
          _:
            _:
              url;
        scopedName = scope:
          name:
            { inherit scope name; };
        ir = identityRegistry;
        l = nodeFileLocalPackage;
        f = nodeFilePackage;
        g = nodeGitPackage;
        n = registries.npm;
        y = registries.yarn;
        sc = scopedName;
        s = self;
      in {
        "@babel/code-frame@7.16.7" = f (sc "babel" "code-frame") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/code-frame/-/code-frame-7.16.7.tgz") "44416b6bd7624b998f5b1af5d470856c40138789" [
          (s."@babel/highlight@^7.16.7")
          ];
        "@babel/code-frame@^7.0.0" = s."@babel/code-frame@7.16.7";
        "@babel/code-frame@^7.10.4" = s."@babel/code-frame@7.16.7";
        "@babel/code-frame@^7.16.7" = s."@babel/code-frame@7.16.7";
        "@babel/code-frame@^7.5.5" = s."@babel/code-frame@7.16.7";
        "@babel/code-frame@^7.8.3" = s."@babel/code-frame@7.16.7";
        "@babel/compat-data@7.16.8" = f (sc "babel" "compat-data") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/compat-data/-/compat-data-7.16.8.tgz") "31560f9f29fdf1868de8cb55049538a1b9732a60" [];
        "@babel/compat-data@^7.13.11" = s."@babel/compat-data@7.16.8";
        "@babel/compat-data@^7.16.4" = s."@babel/compat-data@7.16.8";
        "@babel/compat-data@^7.16.8" = s."@babel/compat-data@7.16.8";
        "@babel/core@7.12.9" = f (sc "babel" "core") "7.12.9" (ir "https://registry.yarnpkg.com/@babel/core/-/core-7.12.9.tgz") "fd450c4ec10cdbb980e2928b7aa7a28484593fc8" [
          (s."@babel/code-frame@^7.10.4")
          (s."@babel/generator@^7.12.5")
          (s."@babel/helper-module-transforms@^7.12.1")
          (s."@babel/helpers@^7.12.5")
          (s."@babel/parser@^7.12.7")
          (s."@babel/template@^7.12.7")
          (s."@babel/traverse@^7.12.9")
          (s."@babel/types@^7.12.7")
          (s."convert-source-map@^1.7.0")
          (s."debug@^4.1.0")
          (s."gensync@^1.0.0-beta.1")
          (s."json5@^2.1.2")
          (s."lodash@^4.17.19")
          (s."resolve@^1.3.2")
          (s."semver@^5.4.1")
          (s."source-map@^0.5.0")
          ];
        "@babel/core@7.16.12" = f (sc "babel" "core") "7.16.12" (ir "https://registry.yarnpkg.com/@babel/core/-/core-7.16.12.tgz") "5edc53c1b71e54881315923ae2aedea2522bb784" [
          (s."@babel/code-frame@^7.16.7")
          (s."@babel/generator@^7.16.8")
          (s."@babel/helper-compilation-targets@^7.16.7")
          (s."@babel/helper-module-transforms@^7.16.7")
          (s."@babel/helpers@^7.16.7")
          (s."@babel/parser@^7.16.12")
          (s."@babel/template@^7.16.7")
          (s."@babel/traverse@^7.16.10")
          (s."@babel/types@^7.16.8")
          (s."convert-source-map@^1.7.0")
          (s."debug@^4.1.0")
          (s."gensync@^1.0.0-beta.2")
          (s."json5@^2.1.2")
          (s."semver@^6.3.0")
          (s."source-map@^0.5.0")
          ];
        "@babel/core@^7.1.0" = s."@babel/core@7.16.12";
        "@babel/core@^7.12.10" = s."@babel/core@7.16.12";
        "@babel/core@^7.12.3" = s."@babel/core@7.16.12";
        "@babel/core@^7.16.12" = s."@babel/core@7.16.12";
        "@babel/core@^7.7.5" = s."@babel/core@7.16.12";
        "@babel/generator@7.16.8" = f (sc "babel" "generator") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/generator/-/generator-7.16.8.tgz") "359d44d966b8cd059d543250ce79596f792f2ebe" [
          (s."@babel/types@^7.16.8")
          (s."jsesc@^2.5.1")
          (s."source-map@^0.5.0")
          ];
        "@babel/generator@^7.12.11" = s."@babel/generator@7.16.8";
        "@babel/generator@^7.12.5" = s."@babel/generator@7.16.8";
        "@babel/generator@^7.16.8" = s."@babel/generator@7.16.8";
        "@babel/helper-annotate-as-pure@7.16.7" = f (sc "babel" "helper-annotate-as-pure") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-annotate-as-pure/-/helper-annotate-as-pure-7.16.7.tgz") "bb2339a7534a9c128e3102024c60760a3a7f3862" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-annotate-as-pure@^7.16.7" = s."@babel/helper-annotate-as-pure@7.16.7";
        "@babel/helper-builder-binary-assignment-operator-visitor@7.16.7" = f (sc "babel" "helper-builder-binary-assignment-operator-visitor") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-builder-binary-assignment-operator-visitor/-/helper-builder-binary-assignment-operator-visitor-7.16.7.tgz") "38d138561ea207f0f69eb1626a418e4f7e6a580b" [
          (s."@babel/helper-explode-assignable-expression@^7.16.7")
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-builder-binary-assignment-operator-visitor@^7.16.7" = s."@babel/helper-builder-binary-assignment-operator-visitor@7.16.7";
        "@babel/helper-compilation-targets@7.16.7" = f (sc "babel" "helper-compilation-targets") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-compilation-targets/-/helper-compilation-targets-7.16.7.tgz") "06e66c5f299601e6c7da350049315e83209d551b" [
          (s."@babel/compat-data@^7.16.4")
          (s."@babel/helper-validator-option@^7.16.7")
          (s."browserslist@^4.17.5")
          (s."semver@^6.3.0")
          ];
        "@babel/helper-compilation-targets@^7.13.0" = s."@babel/helper-compilation-targets@7.16.7";
        "@babel/helper-compilation-targets@^7.16.7" = s."@babel/helper-compilation-targets@7.16.7";
        "@babel/helper-create-class-features-plugin@7.16.10" = f (sc "babel" "helper-create-class-features-plugin") "7.16.10" (ir "https://registry.yarnpkg.com/@babel/helper-create-class-features-plugin/-/helper-create-class-features-plugin-7.16.10.tgz") "8a6959b9cc818a88815ba3c5474619e9c0f2c21c" [
          (s."@babel/helper-annotate-as-pure@^7.16.7")
          (s."@babel/helper-environment-visitor@^7.16.7")
          (s."@babel/helper-function-name@^7.16.7")
          (s."@babel/helper-member-expression-to-functions@^7.16.7")
          (s."@babel/helper-optimise-call-expression@^7.16.7")
          (s."@babel/helper-replace-supers@^7.16.7")
          (s."@babel/helper-split-export-declaration@^7.16.7")
          ];
        "@babel/helper-create-class-features-plugin@^7.16.10" = s."@babel/helper-create-class-features-plugin@7.16.10";
        "@babel/helper-create-class-features-plugin@^7.16.7" = s."@babel/helper-create-class-features-plugin@7.16.10";
        "@babel/helper-create-regexp-features-plugin@7.16.7" = f (sc "babel" "helper-create-regexp-features-plugin") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-create-regexp-features-plugin/-/helper-create-regexp-features-plugin-7.16.7.tgz") "0cb82b9bac358eb73bfbd73985a776bfa6b14d48" [
          (s."@babel/helper-annotate-as-pure@^7.16.7")
          (s."regexpu-core@^4.7.1")
          ];
        "@babel/helper-create-regexp-features-plugin@^7.16.7" = s."@babel/helper-create-regexp-features-plugin@7.16.7";
        "@babel/helper-define-polyfill-provider@0.1.5" = f (sc "babel" "helper-define-polyfill-provider") "0.1.5" (ir "https://registry.yarnpkg.com/@babel/helper-define-polyfill-provider/-/helper-define-polyfill-provider-0.1.5.tgz") "3c2f91b7971b9fc11fe779c945c014065dea340e" [
          (s."@babel/helper-compilation-targets@^7.13.0")
          (s."@babel/helper-module-imports@^7.12.13")
          (s."@babel/helper-plugin-utils@^7.13.0")
          (s."@babel/traverse@^7.13.0")
          (s."debug@^4.1.1")
          (s."lodash.debounce@^4.0.8")
          (s."resolve@^1.14.2")
          (s."semver@^6.1.2")
          ];
        "@babel/helper-define-polyfill-provider@0.3.1" = f (sc "babel" "helper-define-polyfill-provider") "0.3.1" (ir "https://registry.yarnpkg.com/@babel/helper-define-polyfill-provider/-/helper-define-polyfill-provider-0.3.1.tgz") "52411b445bdb2e676869e5a74960d2d3826d2665" [
          (s."@babel/helper-compilation-targets@^7.13.0")
          (s."@babel/helper-module-imports@^7.12.13")
          (s."@babel/helper-plugin-utils@^7.13.0")
          (s."@babel/traverse@^7.13.0")
          (s."debug@^4.1.1")
          (s."lodash.debounce@^4.0.8")
          (s."resolve@^1.14.2")
          (s."semver@^6.1.2")
          ];
        "@babel/helper-define-polyfill-provider@^0.1.5" = s."@babel/helper-define-polyfill-provider@0.1.5";
        "@babel/helper-define-polyfill-provider@^0.3.1" = s."@babel/helper-define-polyfill-provider@0.3.1";
        "@babel/helper-environment-visitor@7.16.7" = f (sc "babel" "helper-environment-visitor") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-environment-visitor/-/helper-environment-visitor-7.16.7.tgz") "ff484094a839bde9d89cd63cba017d7aae80ecd7" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-environment-visitor@^7.16.7" = s."@babel/helper-environment-visitor@7.16.7";
        "@babel/helper-explode-assignable-expression@7.16.7" = f (sc "babel" "helper-explode-assignable-expression") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-explode-assignable-expression/-/helper-explode-assignable-expression-7.16.7.tgz") "12a6d8522fdd834f194e868af6354e8650242b7a" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-explode-assignable-expression@^7.16.7" = s."@babel/helper-explode-assignable-expression@7.16.7";
        "@babel/helper-function-name@7.16.7" = f (sc "babel" "helper-function-name") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-function-name/-/helper-function-name-7.16.7.tgz") "f1ec51551fb1c8956bc8dd95f38523b6cf375f8f" [
          (s."@babel/helper-get-function-arity@^7.16.7")
          (s."@babel/template@^7.16.7")
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-function-name@^7.16.7" = s."@babel/helper-function-name@7.16.7";
        "@babel/helper-get-function-arity@7.16.7" = f (sc "babel" "helper-get-function-arity") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-get-function-arity/-/helper-get-function-arity-7.16.7.tgz") "ea08ac753117a669f1508ba06ebcc49156387419" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-get-function-arity@^7.16.7" = s."@babel/helper-get-function-arity@7.16.7";
        "@babel/helper-hoist-variables@7.16.7" = f (sc "babel" "helper-hoist-variables") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-hoist-variables/-/helper-hoist-variables-7.16.7.tgz") "86bcb19a77a509c7b77d0e22323ef588fa58c246" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-hoist-variables@^7.16.7" = s."@babel/helper-hoist-variables@7.16.7";
        "@babel/helper-member-expression-to-functions@7.16.7" = f (sc "babel" "helper-member-expression-to-functions") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-member-expression-to-functions/-/helper-member-expression-to-functions-7.16.7.tgz") "42b9ca4b2b200123c3b7e726b0ae5153924905b0" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-member-expression-to-functions@^7.16.7" = s."@babel/helper-member-expression-to-functions@7.16.7";
        "@babel/helper-module-imports@7.16.7" = f (sc "babel" "helper-module-imports") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-module-imports/-/helper-module-imports-7.16.7.tgz") "25612a8091a999704461c8a222d0efec5d091437" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-module-imports@^7.0.0" = s."@babel/helper-module-imports@7.16.7";
        "@babel/helper-module-imports@^7.12.13" = s."@babel/helper-module-imports@7.16.7";
        "@babel/helper-module-imports@^7.16.7" = s."@babel/helper-module-imports@7.16.7";
        "@babel/helper-module-transforms@7.16.7" = f (sc "babel" "helper-module-transforms") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-module-transforms/-/helper-module-transforms-7.16.7.tgz") "7665faeb721a01ca5327ddc6bba15a5cb34b6a41" [
          (s."@babel/helper-environment-visitor@^7.16.7")
          (s."@babel/helper-module-imports@^7.16.7")
          (s."@babel/helper-simple-access@^7.16.7")
          (s."@babel/helper-split-export-declaration@^7.16.7")
          (s."@babel/helper-validator-identifier@^7.16.7")
          (s."@babel/template@^7.16.7")
          (s."@babel/traverse@^7.16.7")
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-module-transforms@^7.12.1" = s."@babel/helper-module-transforms@7.16.7";
        "@babel/helper-module-transforms@^7.16.7" = s."@babel/helper-module-transforms@7.16.7";
        "@babel/helper-optimise-call-expression@7.16.7" = f (sc "babel" "helper-optimise-call-expression") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-optimise-call-expression/-/helper-optimise-call-expression-7.16.7.tgz") "a34e3560605abbd31a18546bd2aad3e6d9a174f2" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-optimise-call-expression@^7.16.7" = s."@babel/helper-optimise-call-expression@7.16.7";
        "@babel/helper-plugin-utils@7.10.4" = f (sc "babel" "helper-plugin-utils") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/helper-plugin-utils/-/helper-plugin-utils-7.10.4.tgz") "2f75a831269d4f677de49986dff59927533cf375" [];
        "@babel/helper-plugin-utils@7.16.7" = f (sc "babel" "helper-plugin-utils") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-plugin-utils/-/helper-plugin-utils-7.16.7.tgz") "aa3a8ab4c3cceff8e65eb9e73d87dc4ff320b2f5" [];
        "@babel/helper-plugin-utils@^7.0.0" = s."@babel/helper-plugin-utils@7.16.7";
        "@babel/helper-plugin-utils@^7.10.4" = s."@babel/helper-plugin-utils@7.16.7";
        "@babel/helper-plugin-utils@^7.12.13" = s."@babel/helper-plugin-utils@7.16.7";
        "@babel/helper-plugin-utils@^7.13.0" = s."@babel/helper-plugin-utils@7.16.7";
        "@babel/helper-plugin-utils@^7.14.5" = s."@babel/helper-plugin-utils@7.16.7";
        "@babel/helper-plugin-utils@^7.16.7" = s."@babel/helper-plugin-utils@7.16.7";
        "@babel/helper-plugin-utils@^7.8.0" = s."@babel/helper-plugin-utils@7.16.7";
        "@babel/helper-plugin-utils@^7.8.3" = s."@babel/helper-plugin-utils@7.16.7";
        "@babel/helper-remap-async-to-generator@7.16.8" = f (sc "babel" "helper-remap-async-to-generator") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/helper-remap-async-to-generator/-/helper-remap-async-to-generator-7.16.8.tgz") "29ffaade68a367e2ed09c90901986918d25e57e3" [
          (s."@babel/helper-annotate-as-pure@^7.16.7")
          (s."@babel/helper-wrap-function@^7.16.8")
          (s."@babel/types@^7.16.8")
          ];
        "@babel/helper-remap-async-to-generator@^7.16.8" = s."@babel/helper-remap-async-to-generator@7.16.8";
        "@babel/helper-replace-supers@7.16.7" = f (sc "babel" "helper-replace-supers") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-replace-supers/-/helper-replace-supers-7.16.7.tgz") "e9f5f5f32ac90429c1a4bdec0f231ef0c2838ab1" [
          (s."@babel/helper-environment-visitor@^7.16.7")
          (s."@babel/helper-member-expression-to-functions@^7.16.7")
          (s."@babel/helper-optimise-call-expression@^7.16.7")
          (s."@babel/traverse@^7.16.7")
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-replace-supers@^7.16.7" = s."@babel/helper-replace-supers@7.16.7";
        "@babel/helper-simple-access@7.16.7" = f (sc "babel" "helper-simple-access") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-simple-access/-/helper-simple-access-7.16.7.tgz") "d656654b9ea08dbb9659b69d61063ccd343ff0f7" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-simple-access@^7.16.7" = s."@babel/helper-simple-access@7.16.7";
        "@babel/helper-skip-transparent-expression-wrappers@7.16.0" = f (sc "babel" "helper-skip-transparent-expression-wrappers") "7.16.0" (ir "https://registry.yarnpkg.com/@babel/helper-skip-transparent-expression-wrappers/-/helper-skip-transparent-expression-wrappers-7.16.0.tgz") "0ee3388070147c3ae051e487eca3ebb0e2e8bb09" [
          (s."@babel/types@^7.16.0")
          ];
        "@babel/helper-skip-transparent-expression-wrappers@^7.16.0" = s."@babel/helper-skip-transparent-expression-wrappers@7.16.0";
        "@babel/helper-split-export-declaration@7.16.7" = f (sc "babel" "helper-split-export-declaration") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-split-export-declaration/-/helper-split-export-declaration-7.16.7.tgz") "0b648c0c42da9d3920d85ad585f2778620b8726b" [
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helper-split-export-declaration@^7.16.7" = s."@babel/helper-split-export-declaration@7.16.7";
        "@babel/helper-validator-identifier@7.16.7" = f (sc "babel" "helper-validator-identifier") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-validator-identifier/-/helper-validator-identifier-7.16.7.tgz") "e8c602438c4a8195751243da9031d1607d247cad" [];
        "@babel/helper-validator-identifier@^7.16.7" = s."@babel/helper-validator-identifier@7.16.7";
        "@babel/helper-validator-option@7.16.7" = f (sc "babel" "helper-validator-option") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helper-validator-option/-/helper-validator-option-7.16.7.tgz") "b203ce62ce5fe153899b617c08957de860de4d23" [];
        "@babel/helper-validator-option@^7.16.7" = s."@babel/helper-validator-option@7.16.7";
        "@babel/helper-wrap-function@7.16.8" = f (sc "babel" "helper-wrap-function") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/helper-wrap-function/-/helper-wrap-function-7.16.8.tgz") "58afda087c4cd235de92f7ceedebca2c41274200" [
          (s."@babel/helper-function-name@^7.16.7")
          (s."@babel/template@^7.16.7")
          (s."@babel/traverse@^7.16.8")
          (s."@babel/types@^7.16.8")
          ];
        "@babel/helper-wrap-function@^7.16.8" = s."@babel/helper-wrap-function@7.16.8";
        "@babel/helpers@7.16.7" = f (sc "babel" "helpers") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/helpers/-/helpers-7.16.7.tgz") "7e3504d708d50344112767c3542fc5e357fffefc" [
          (s."@babel/template@^7.16.7")
          (s."@babel/traverse@^7.16.7")
          (s."@babel/types@^7.16.7")
          ];
        "@babel/helpers@^7.12.5" = s."@babel/helpers@7.16.7";
        "@babel/helpers@^7.16.7" = s."@babel/helpers@7.16.7";
        "@babel/highlight@7.16.10" = f (sc "babel" "highlight") "7.16.10" (ir "https://registry.yarnpkg.com/@babel/highlight/-/highlight-7.16.10.tgz") "744f2eb81579d6eea753c227b0f570ad785aba88" [
          (s."@babel/helper-validator-identifier@^7.16.7")
          (s."chalk@^2.0.0")
          (s."js-tokens@^4.0.0")
          ];
        "@babel/highlight@^7.16.7" = s."@babel/highlight@7.16.10";
        "@babel/parser@7.16.12" = f (sc "babel" "parser") "7.16.12" (ir "https://registry.yarnpkg.com/@babel/parser/-/parser-7.16.12.tgz") "9474794f9a650cf5e2f892444227f98e28cdf8b6" [];
        "@babel/parser@^7.12.11" = s."@babel/parser@7.16.12";
        "@babel/parser@^7.12.7" = s."@babel/parser@7.16.12";
        "@babel/parser@^7.14.7" = s."@babel/parser@7.16.12";
        "@babel/parser@^7.16.10" = s."@babel/parser@7.16.12";
        "@babel/parser@^7.16.12" = s."@babel/parser@7.16.12";
        "@babel/parser@^7.16.7" = s."@babel/parser@7.16.12";
        "@babel/plugin-bugfix-safari-id-destructuring-collision-in-function-expression@7.16.7" = f (sc "babel" "plugin-bugfix-safari-id-destructuring-collision-in-function-expression") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-bugfix-safari-id-destructuring-collision-in-function-expression/-/plugin-bugfix-safari-id-destructuring-collision-in-function-expression-7.16.7.tgz") "4eda6d6c2a0aa79c70fa7b6da67763dfe2141050" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-bugfix-safari-id-destructuring-collision-in-function-expression@^7.16.7" = s."@babel/plugin-bugfix-safari-id-destructuring-collision-in-function-expression@7.16.7";
        "@babel/plugin-bugfix-v8-spread-parameters-in-optional-chaining@7.16.7" = f (sc "babel" "plugin-bugfix-v8-spread-parameters-in-optional-chaining") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-bugfix-v8-spread-parameters-in-optional-chaining/-/plugin-bugfix-v8-spread-parameters-in-optional-chaining-7.16.7.tgz") "cc001234dfc139ac45f6bcf801866198c8c72ff9" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-skip-transparent-expression-wrappers@^7.16.0")
          (s."@babel/plugin-proposal-optional-chaining@^7.16.7")
          ];
        "@babel/plugin-bugfix-v8-spread-parameters-in-optional-chaining@^7.16.7" = s."@babel/plugin-bugfix-v8-spread-parameters-in-optional-chaining@7.16.7";
        "@babel/plugin-proposal-async-generator-functions@7.16.8" = f (sc "babel" "plugin-proposal-async-generator-functions") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-async-generator-functions/-/plugin-proposal-async-generator-functions-7.16.8.tgz") "3bdd1ebbe620804ea9416706cd67d60787504bc8" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-remap-async-to-generator@^7.16.8")
          (s."@babel/plugin-syntax-async-generators@^7.8.4")
          ];
        "@babel/plugin-proposal-async-generator-functions@^7.16.8" = s."@babel/plugin-proposal-async-generator-functions@7.16.8";
        "@babel/plugin-proposal-class-properties@7.16.7" = f (sc "babel" "plugin-proposal-class-properties") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-class-properties/-/plugin-proposal-class-properties-7.16.7.tgz") "925cad7b3b1a2fcea7e59ecc8eb5954f961f91b0" [
          (s."@babel/helper-create-class-features-plugin@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-proposal-class-properties@^7.12.1" = s."@babel/plugin-proposal-class-properties@7.16.7";
        "@babel/plugin-proposal-class-properties@^7.16.7" = s."@babel/plugin-proposal-class-properties@7.16.7";
        "@babel/plugin-proposal-class-static-block@7.16.7" = f (sc "babel" "plugin-proposal-class-static-block") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-class-static-block/-/plugin-proposal-class-static-block-7.16.7.tgz") "712357570b612106ef5426d13dc433ce0f200c2a" [
          (s."@babel/helper-create-class-features-plugin@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-class-static-block@^7.14.5")
          ];
        "@babel/plugin-proposal-class-static-block@^7.16.7" = s."@babel/plugin-proposal-class-static-block@7.16.7";
        "@babel/plugin-proposal-decorators@7.16.7" = f (sc "babel" "plugin-proposal-decorators") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-decorators/-/plugin-proposal-decorators-7.16.7.tgz") "922907d2e3e327f5b07d2246bcfc0bd438f360d2" [
          (s."@babel/helper-create-class-features-plugin@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-decorators@^7.16.7")
          ];
        "@babel/plugin-proposal-decorators@^7.12.12" = s."@babel/plugin-proposal-decorators@7.16.7";
        "@babel/plugin-proposal-dynamic-import@7.16.7" = f (sc "babel" "plugin-proposal-dynamic-import") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-dynamic-import/-/plugin-proposal-dynamic-import-7.16.7.tgz") "c19c897eaa46b27634a00fee9fb7d829158704b2" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-dynamic-import@^7.8.3")
          ];
        "@babel/plugin-proposal-dynamic-import@^7.16.7" = s."@babel/plugin-proposal-dynamic-import@7.16.7";
        "@babel/plugin-proposal-export-default-from@7.16.7" = f (sc "babel" "plugin-proposal-export-default-from") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-export-default-from/-/plugin-proposal-export-default-from-7.16.7.tgz") "a40ab158ca55627b71c5513f03d3469026a9e929" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-export-default-from@^7.16.7")
          ];
        "@babel/plugin-proposal-export-default-from@^7.12.1" = s."@babel/plugin-proposal-export-default-from@7.16.7";
        "@babel/plugin-proposal-export-namespace-from@7.16.7" = f (sc "babel" "plugin-proposal-export-namespace-from") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-export-namespace-from/-/plugin-proposal-export-namespace-from-7.16.7.tgz") "09de09df18445a5786a305681423ae63507a6163" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-export-namespace-from@^7.8.3")
          ];
        "@babel/plugin-proposal-export-namespace-from@^7.16.7" = s."@babel/plugin-proposal-export-namespace-from@7.16.7";
        "@babel/plugin-proposal-json-strings@7.16.7" = f (sc "babel" "plugin-proposal-json-strings") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-json-strings/-/plugin-proposal-json-strings-7.16.7.tgz") "9732cb1d17d9a2626a08c5be25186c195b6fa6e8" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-json-strings@^7.8.3")
          ];
        "@babel/plugin-proposal-json-strings@^7.16.7" = s."@babel/plugin-proposal-json-strings@7.16.7";
        "@babel/plugin-proposal-logical-assignment-operators@7.16.7" = f (sc "babel" "plugin-proposal-logical-assignment-operators") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-logical-assignment-operators/-/plugin-proposal-logical-assignment-operators-7.16.7.tgz") "be23c0ba74deec1922e639832904be0bea73cdea" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-logical-assignment-operators@^7.10.4")
          ];
        "@babel/plugin-proposal-logical-assignment-operators@^7.16.7" = s."@babel/plugin-proposal-logical-assignment-operators@7.16.7";
        "@babel/plugin-proposal-nullish-coalescing-operator@7.16.7" = f (sc "babel" "plugin-proposal-nullish-coalescing-operator") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-nullish-coalescing-operator/-/plugin-proposal-nullish-coalescing-operator-7.16.7.tgz") "141fc20b6857e59459d430c850a0011e36561d99" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-nullish-coalescing-operator@^7.8.3")
          ];
        "@babel/plugin-proposal-nullish-coalescing-operator@^7.12.1" = s."@babel/plugin-proposal-nullish-coalescing-operator@7.16.7";
        "@babel/plugin-proposal-nullish-coalescing-operator@^7.16.7" = s."@babel/plugin-proposal-nullish-coalescing-operator@7.16.7";
        "@babel/plugin-proposal-numeric-separator@7.16.7" = f (sc "babel" "plugin-proposal-numeric-separator") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-numeric-separator/-/plugin-proposal-numeric-separator-7.16.7.tgz") "d6b69f4af63fb38b6ca2558442a7fb191236eba9" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-numeric-separator@^7.10.4")
          ];
        "@babel/plugin-proposal-numeric-separator@^7.16.7" = s."@babel/plugin-proposal-numeric-separator@7.16.7";
        "@babel/plugin-proposal-object-rest-spread@7.12.1" = f (sc "babel" "plugin-proposal-object-rest-spread") "7.12.1" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-object-rest-spread/-/plugin-proposal-object-rest-spread-7.12.1.tgz") "def9bd03cea0f9b72283dac0ec22d289c7691069" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          (s."@babel/plugin-syntax-object-rest-spread@^7.8.0")
          (s."@babel/plugin-transform-parameters@^7.12.1")
          ];
        "@babel/plugin-proposal-object-rest-spread@7.16.7" = f (sc "babel" "plugin-proposal-object-rest-spread") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-object-rest-spread/-/plugin-proposal-object-rest-spread-7.16.7.tgz") "94593ef1ddf37021a25bdcb5754c4a8d534b01d8" [
          (s."@babel/compat-data@^7.16.4")
          (s."@babel/helper-compilation-targets@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-object-rest-spread@^7.8.3")
          (s."@babel/plugin-transform-parameters@^7.16.7")
          ];
        "@babel/plugin-proposal-object-rest-spread@^7.12.1" = s."@babel/plugin-proposal-object-rest-spread@7.16.7";
        "@babel/plugin-proposal-object-rest-spread@^7.16.7" = s."@babel/plugin-proposal-object-rest-spread@7.16.7";
        "@babel/plugin-proposal-optional-catch-binding@7.16.7" = f (sc "babel" "plugin-proposal-optional-catch-binding") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-optional-catch-binding/-/plugin-proposal-optional-catch-binding-7.16.7.tgz") "c623a430674ffc4ab732fd0a0ae7722b67cb74cf" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-optional-catch-binding@^7.8.3")
          ];
        "@babel/plugin-proposal-optional-catch-binding@^7.16.7" = s."@babel/plugin-proposal-optional-catch-binding@7.16.7";
        "@babel/plugin-proposal-optional-chaining@7.16.7" = f (sc "babel" "plugin-proposal-optional-chaining") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-optional-chaining/-/plugin-proposal-optional-chaining-7.16.7.tgz") "7cd629564724816c0e8a969535551f943c64c39a" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-skip-transparent-expression-wrappers@^7.16.0")
          (s."@babel/plugin-syntax-optional-chaining@^7.8.3")
          ];
        "@babel/plugin-proposal-optional-chaining@^7.12.7" = s."@babel/plugin-proposal-optional-chaining@7.16.7";
        "@babel/plugin-proposal-optional-chaining@^7.16.7" = s."@babel/plugin-proposal-optional-chaining@7.16.7";
        "@babel/plugin-proposal-private-methods@7.16.11" = f (sc "babel" "plugin-proposal-private-methods") "7.16.11" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-private-methods/-/plugin-proposal-private-methods-7.16.11.tgz") "e8df108288555ff259f4527dbe84813aac3a1c50" [
          (s."@babel/helper-create-class-features-plugin@^7.16.10")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-proposal-private-methods@^7.12.1" = s."@babel/plugin-proposal-private-methods@7.16.11";
        "@babel/plugin-proposal-private-methods@^7.16.11" = s."@babel/plugin-proposal-private-methods@7.16.11";
        "@babel/plugin-proposal-private-property-in-object@7.16.7" = f (sc "babel" "plugin-proposal-private-property-in-object") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-private-property-in-object/-/plugin-proposal-private-property-in-object-7.16.7.tgz") "b0b8cef543c2c3d57e59e2c611994861d46a3fce" [
          (s."@babel/helper-annotate-as-pure@^7.16.7")
          (s."@babel/helper-create-class-features-plugin@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-private-property-in-object@^7.14.5")
          ];
        "@babel/plugin-proposal-private-property-in-object@^7.16.7" = s."@babel/plugin-proposal-private-property-in-object@7.16.7";
        "@babel/plugin-proposal-unicode-property-regex@7.16.7" = f (sc "babel" "plugin-proposal-unicode-property-regex") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-proposal-unicode-property-regex/-/plugin-proposal-unicode-property-regex-7.16.7.tgz") "635d18eb10c6214210ffc5ff4932552de08188a2" [
          (s."@babel/helper-create-regexp-features-plugin@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-proposal-unicode-property-regex@^7.16.7" = s."@babel/plugin-proposal-unicode-property-regex@7.16.7";
        "@babel/plugin-proposal-unicode-property-regex@^7.4.4" = s."@babel/plugin-proposal-unicode-property-regex@7.16.7";
        "@babel/plugin-syntax-async-generators@7.8.4" = f (sc "babel" "plugin-syntax-async-generators") "7.8.4" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-async-generators/-/plugin-syntax-async-generators-7.8.4.tgz") "a983fb1aeb2ec3f6ed042a210f640e90e786fe0d" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-async-generators@^7.8.4" = s."@babel/plugin-syntax-async-generators@7.8.4";
        "@babel/plugin-syntax-class-properties@7.12.13" = f (sc "babel" "plugin-syntax-class-properties") "7.12.13" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-class-properties/-/plugin-syntax-class-properties-7.12.13.tgz") "b5c987274c4a3a82b89714796931a6b53544ae10" [
          (s."@babel/helper-plugin-utils@^7.12.13")
          ];
        "@babel/plugin-syntax-class-properties@^7.12.13" = s."@babel/plugin-syntax-class-properties@7.12.13";
        "@babel/plugin-syntax-class-static-block@7.14.5" = f (sc "babel" "plugin-syntax-class-static-block") "7.14.5" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-class-static-block/-/plugin-syntax-class-static-block-7.14.5.tgz") "195df89b146b4b78b3bf897fd7a257c84659d406" [
          (s."@babel/helper-plugin-utils@^7.14.5")
          ];
        "@babel/plugin-syntax-class-static-block@^7.14.5" = s."@babel/plugin-syntax-class-static-block@7.14.5";
        "@babel/plugin-syntax-decorators@7.16.7" = f (sc "babel" "plugin-syntax-decorators") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-decorators/-/plugin-syntax-decorators-7.16.7.tgz") "f66a0199f16de7c1ef5192160ccf5d069739e3d3" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-syntax-decorators@^7.16.7" = s."@babel/plugin-syntax-decorators@7.16.7";
        "@babel/plugin-syntax-dynamic-import@7.8.3" = f (sc "babel" "plugin-syntax-dynamic-import") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-dynamic-import/-/plugin-syntax-dynamic-import-7.8.3.tgz") "62bf98b2da3cd21d626154fc96ee5b3cb68eacb3" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-dynamic-import@^7.8.3" = s."@babel/plugin-syntax-dynamic-import@7.8.3";
        "@babel/plugin-syntax-export-default-from@7.16.7" = f (sc "babel" "plugin-syntax-export-default-from") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-export-default-from/-/plugin-syntax-export-default-from-7.16.7.tgz") "fa89cf13b60de2c3f79acdc2b52a21174c6de060" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-syntax-export-default-from@^7.16.7" = s."@babel/plugin-syntax-export-default-from@7.16.7";
        "@babel/plugin-syntax-export-namespace-from@7.8.3" = f (sc "babel" "plugin-syntax-export-namespace-from") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-export-namespace-from/-/plugin-syntax-export-namespace-from-7.8.3.tgz") "028964a9ba80dbc094c915c487ad7c4e7a66465a" [
          (s."@babel/helper-plugin-utils@^7.8.3")
          ];
        "@babel/plugin-syntax-export-namespace-from@^7.8.3" = s."@babel/plugin-syntax-export-namespace-from@7.8.3";
        "@babel/plugin-syntax-flow@7.16.7" = f (sc "babel" "plugin-syntax-flow") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-flow/-/plugin-syntax-flow-7.16.7.tgz") "202b147e5892b8452bbb0bb269c7ed2539ab8832" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-syntax-flow@^7.16.7" = s."@babel/plugin-syntax-flow@7.16.7";
        "@babel/plugin-syntax-json-strings@7.8.3" = f (sc "babel" "plugin-syntax-json-strings") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-json-strings/-/plugin-syntax-json-strings-7.8.3.tgz") "01ca21b668cd8218c9e640cb6dd88c5412b2c96a" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-json-strings@^7.8.3" = s."@babel/plugin-syntax-json-strings@7.8.3";
        "@babel/plugin-syntax-jsx@7.12.1" = f (sc "babel" "plugin-syntax-jsx") "7.12.1" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-jsx/-/plugin-syntax-jsx-7.12.1.tgz") "9d9d357cc818aa7ae7935917c1257f67677a0926" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-syntax-jsx@7.16.7" = f (sc "babel" "plugin-syntax-jsx") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-jsx/-/plugin-syntax-jsx-7.16.7.tgz") "50b6571d13f764266a113d77c82b4a6508bbe665" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-syntax-jsx@^7.16.7" = s."@babel/plugin-syntax-jsx@7.16.7";
        "@babel/plugin-syntax-logical-assignment-operators@7.10.4" = f (sc "babel" "plugin-syntax-logical-assignment-operators") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-logical-assignment-operators/-/plugin-syntax-logical-assignment-operators-7.10.4.tgz") "ca91ef46303530448b906652bac2e9fe9941f699" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-syntax-logical-assignment-operators@^7.10.4" = s."@babel/plugin-syntax-logical-assignment-operators@7.10.4";
        "@babel/plugin-syntax-nullish-coalescing-operator@7.8.3" = f (sc "babel" "plugin-syntax-nullish-coalescing-operator") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-nullish-coalescing-operator/-/plugin-syntax-nullish-coalescing-operator-7.8.3.tgz") "167ed70368886081f74b5c36c65a88c03b66d1a9" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-nullish-coalescing-operator@^7.8.3" = s."@babel/plugin-syntax-nullish-coalescing-operator@7.8.3";
        "@babel/plugin-syntax-numeric-separator@7.10.4" = f (sc "babel" "plugin-syntax-numeric-separator") "7.10.4" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-numeric-separator/-/plugin-syntax-numeric-separator-7.10.4.tgz") "b9b070b3e33570cd9fd07ba7fa91c0dd37b9af97" [
          (s."@babel/helper-plugin-utils@^7.10.4")
          ];
        "@babel/plugin-syntax-numeric-separator@^7.10.4" = s."@babel/plugin-syntax-numeric-separator@7.10.4";
        "@babel/plugin-syntax-object-rest-spread@7.8.3" = f (sc "babel" "plugin-syntax-object-rest-spread") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-object-rest-spread/-/plugin-syntax-object-rest-spread-7.8.3.tgz") "60e225edcbd98a640332a2e72dd3e66f1af55871" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-object-rest-spread@^7.8.0" = s."@babel/plugin-syntax-object-rest-spread@7.8.3";
        "@babel/plugin-syntax-object-rest-spread@^7.8.3" = s."@babel/plugin-syntax-object-rest-spread@7.8.3";
        "@babel/plugin-syntax-optional-catch-binding@7.8.3" = f (sc "babel" "plugin-syntax-optional-catch-binding") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-optional-catch-binding/-/plugin-syntax-optional-catch-binding-7.8.3.tgz") "6111a265bcfb020eb9efd0fdfd7d26402b9ed6c1" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-optional-catch-binding@^7.8.3" = s."@babel/plugin-syntax-optional-catch-binding@7.8.3";
        "@babel/plugin-syntax-optional-chaining@7.8.3" = f (sc "babel" "plugin-syntax-optional-chaining") "7.8.3" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-optional-chaining/-/plugin-syntax-optional-chaining-7.8.3.tgz") "4f69c2ab95167e0180cd5336613f8c5788f7d48a" [
          (s."@babel/helper-plugin-utils@^7.8.0")
          ];
        "@babel/plugin-syntax-optional-chaining@^7.8.3" = s."@babel/plugin-syntax-optional-chaining@7.8.3";
        "@babel/plugin-syntax-private-property-in-object@7.14.5" = f (sc "babel" "plugin-syntax-private-property-in-object") "7.14.5" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-private-property-in-object/-/plugin-syntax-private-property-in-object-7.14.5.tgz") "0dc6671ec0ea22b6e94a1114f857970cd39de1ad" [
          (s."@babel/helper-plugin-utils@^7.14.5")
          ];
        "@babel/plugin-syntax-private-property-in-object@^7.14.5" = s."@babel/plugin-syntax-private-property-in-object@7.14.5";
        "@babel/plugin-syntax-top-level-await@7.14.5" = f (sc "babel" "plugin-syntax-top-level-await") "7.14.5" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-top-level-await/-/plugin-syntax-top-level-await-7.14.5.tgz") "c1cfdadc35a646240001f06138247b741c34d94c" [
          (s."@babel/helper-plugin-utils@^7.14.5")
          ];
        "@babel/plugin-syntax-top-level-await@^7.14.5" = s."@babel/plugin-syntax-top-level-await@7.14.5";
        "@babel/plugin-syntax-typescript@7.16.7" = f (sc "babel" "plugin-syntax-typescript") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-syntax-typescript/-/plugin-syntax-typescript-7.16.7.tgz") "39c9b55ee153151990fb038651d58d3fd03f98f8" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-syntax-typescript@^7.16.7" = s."@babel/plugin-syntax-typescript@7.16.7";
        "@babel/plugin-transform-arrow-functions@7.16.7" = f (sc "babel" "plugin-transform-arrow-functions") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-arrow-functions/-/plugin-transform-arrow-functions-7.16.7.tgz") "44125e653d94b98db76369de9c396dc14bef4154" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-arrow-functions@^7.12.1" = s."@babel/plugin-transform-arrow-functions@7.16.7";
        "@babel/plugin-transform-arrow-functions@^7.16.7" = s."@babel/plugin-transform-arrow-functions@7.16.7";
        "@babel/plugin-transform-async-to-generator@7.16.8" = f (sc "babel" "plugin-transform-async-to-generator") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-async-to-generator/-/plugin-transform-async-to-generator-7.16.8.tgz") "b83dff4b970cf41f1b819f8b49cc0cfbaa53a808" [
          (s."@babel/helper-module-imports@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-remap-async-to-generator@^7.16.8")
          ];
        "@babel/plugin-transform-async-to-generator@^7.16.8" = s."@babel/plugin-transform-async-to-generator@7.16.8";
        "@babel/plugin-transform-block-scoped-functions@7.16.7" = f (sc "babel" "plugin-transform-block-scoped-functions") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-block-scoped-functions/-/plugin-transform-block-scoped-functions-7.16.7.tgz") "4d0d57d9632ef6062cdf354bb717102ee042a620" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-block-scoped-functions@^7.16.7" = s."@babel/plugin-transform-block-scoped-functions@7.16.7";
        "@babel/plugin-transform-block-scoping@7.16.7" = f (sc "babel" "plugin-transform-block-scoping") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-block-scoping/-/plugin-transform-block-scoping-7.16.7.tgz") "f50664ab99ddeaee5bc681b8f3a6ea9d72ab4f87" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-block-scoping@^7.12.12" = s."@babel/plugin-transform-block-scoping@7.16.7";
        "@babel/plugin-transform-block-scoping@^7.16.7" = s."@babel/plugin-transform-block-scoping@7.16.7";
        "@babel/plugin-transform-classes@7.16.7" = f (sc "babel" "plugin-transform-classes") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-classes/-/plugin-transform-classes-7.16.7.tgz") "8f4b9562850cd973de3b498f1218796eb181ce00" [
          (s."@babel/helper-annotate-as-pure@^7.16.7")
          (s."@babel/helper-environment-visitor@^7.16.7")
          (s."@babel/helper-function-name@^7.16.7")
          (s."@babel/helper-optimise-call-expression@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-replace-supers@^7.16.7")
          (s."@babel/helper-split-export-declaration@^7.16.7")
          (s."globals@^11.1.0")
          ];
        "@babel/plugin-transform-classes@^7.12.1" = s."@babel/plugin-transform-classes@7.16.7";
        "@babel/plugin-transform-classes@^7.16.7" = s."@babel/plugin-transform-classes@7.16.7";
        "@babel/plugin-transform-computed-properties@7.16.7" = f (sc "babel" "plugin-transform-computed-properties") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-computed-properties/-/plugin-transform-computed-properties-7.16.7.tgz") "66dee12e46f61d2aae7a73710f591eb3df616470" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-computed-properties@^7.16.7" = s."@babel/plugin-transform-computed-properties@7.16.7";
        "@babel/plugin-transform-destructuring@7.16.7" = f (sc "babel" "plugin-transform-destructuring") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-destructuring/-/plugin-transform-destructuring-7.16.7.tgz") "ca9588ae2d63978a4c29d3f33282d8603f618e23" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-destructuring@^7.12.1" = s."@babel/plugin-transform-destructuring@7.16.7";
        "@babel/plugin-transform-destructuring@^7.16.7" = s."@babel/plugin-transform-destructuring@7.16.7";
        "@babel/plugin-transform-dotall-regex@7.16.7" = f (sc "babel" "plugin-transform-dotall-regex") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-dotall-regex/-/plugin-transform-dotall-regex-7.16.7.tgz") "6b2d67686fab15fb6a7fd4bd895d5982cfc81241" [
          (s."@babel/helper-create-regexp-features-plugin@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-dotall-regex@^7.16.7" = s."@babel/plugin-transform-dotall-regex@7.16.7";
        "@babel/plugin-transform-dotall-regex@^7.4.4" = s."@babel/plugin-transform-dotall-regex@7.16.7";
        "@babel/plugin-transform-duplicate-keys@7.16.7" = f (sc "babel" "plugin-transform-duplicate-keys") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-duplicate-keys/-/plugin-transform-duplicate-keys-7.16.7.tgz") "2207e9ca8f82a0d36a5a67b6536e7ef8b08823c9" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-duplicate-keys@^7.16.7" = s."@babel/plugin-transform-duplicate-keys@7.16.7";
        "@babel/plugin-transform-exponentiation-operator@7.16.7" = f (sc "babel" "plugin-transform-exponentiation-operator") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-exponentiation-operator/-/plugin-transform-exponentiation-operator-7.16.7.tgz") "efa9862ef97e9e9e5f653f6ddc7b665e8536fe9b" [
          (s."@babel/helper-builder-binary-assignment-operator-visitor@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-exponentiation-operator@^7.16.7" = s."@babel/plugin-transform-exponentiation-operator@7.16.7";
        "@babel/plugin-transform-flow-strip-types@7.16.7" = f (sc "babel" "plugin-transform-flow-strip-types") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-flow-strip-types/-/plugin-transform-flow-strip-types-7.16.7.tgz") "291fb140c78dabbf87f2427e7c7c332b126964b8" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-flow@^7.16.7")
          ];
        "@babel/plugin-transform-flow-strip-types@^7.16.7" = s."@babel/plugin-transform-flow-strip-types@7.16.7";
        "@babel/plugin-transform-for-of@7.16.7" = f (sc "babel" "plugin-transform-for-of") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-for-of/-/plugin-transform-for-of-7.16.7.tgz") "649d639d4617dff502a9a158c479b3b556728d8c" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-for-of@^7.12.1" = s."@babel/plugin-transform-for-of@7.16.7";
        "@babel/plugin-transform-for-of@^7.16.7" = s."@babel/plugin-transform-for-of@7.16.7";
        "@babel/plugin-transform-function-name@7.16.7" = f (sc "babel" "plugin-transform-function-name") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-function-name/-/plugin-transform-function-name-7.16.7.tgz") "5ab34375c64d61d083d7d2f05c38d90b97ec65cf" [
          (s."@babel/helper-compilation-targets@^7.16.7")
          (s."@babel/helper-function-name@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-function-name@^7.16.7" = s."@babel/plugin-transform-function-name@7.16.7";
        "@babel/plugin-transform-literals@7.16.7" = f (sc "babel" "plugin-transform-literals") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-literals/-/plugin-transform-literals-7.16.7.tgz") "254c9618c5ff749e87cb0c0cef1a0a050c0bdab1" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-literals@^7.16.7" = s."@babel/plugin-transform-literals@7.16.7";
        "@babel/plugin-transform-member-expression-literals@7.16.7" = f (sc "babel" "plugin-transform-member-expression-literals") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-member-expression-literals/-/plugin-transform-member-expression-literals-7.16.7.tgz") "6e5dcf906ef8a098e630149d14c867dd28f92384" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-member-expression-literals@^7.16.7" = s."@babel/plugin-transform-member-expression-literals@7.16.7";
        "@babel/plugin-transform-modules-amd@7.16.7" = f (sc "babel" "plugin-transform-modules-amd") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-modules-amd/-/plugin-transform-modules-amd-7.16.7.tgz") "b28d323016a7daaae8609781d1f8c9da42b13186" [
          (s."@babel/helper-module-transforms@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."babel-plugin-dynamic-import-node@^2.3.3")
          ];
        "@babel/plugin-transform-modules-amd@^7.16.7" = s."@babel/plugin-transform-modules-amd@7.16.7";
        "@babel/plugin-transform-modules-commonjs@7.16.8" = f (sc "babel" "plugin-transform-modules-commonjs") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-modules-commonjs/-/plugin-transform-modules-commonjs-7.16.8.tgz") "cdee19aae887b16b9d331009aa9a219af7c86afe" [
          (s."@babel/helper-module-transforms@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-simple-access@^7.16.7")
          (s."babel-plugin-dynamic-import-node@^2.3.3")
          ];
        "@babel/plugin-transform-modules-commonjs@^7.16.8" = s."@babel/plugin-transform-modules-commonjs@7.16.8";
        "@babel/plugin-transform-modules-systemjs@7.16.7" = f (sc "babel" "plugin-transform-modules-systemjs") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-modules-systemjs/-/plugin-transform-modules-systemjs-7.16.7.tgz") "887cefaef88e684d29558c2b13ee0563e287c2d7" [
          (s."@babel/helper-hoist-variables@^7.16.7")
          (s."@babel/helper-module-transforms@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-validator-identifier@^7.16.7")
          (s."babel-plugin-dynamic-import-node@^2.3.3")
          ];
        "@babel/plugin-transform-modules-systemjs@^7.16.7" = s."@babel/plugin-transform-modules-systemjs@7.16.7";
        "@babel/plugin-transform-modules-umd@7.16.7" = f (sc "babel" "plugin-transform-modules-umd") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-modules-umd/-/plugin-transform-modules-umd-7.16.7.tgz") "23dad479fa585283dbd22215bff12719171e7618" [
          (s."@babel/helper-module-transforms@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-modules-umd@^7.16.7" = s."@babel/plugin-transform-modules-umd@7.16.7";
        "@babel/plugin-transform-named-capturing-groups-regex@7.16.8" = f (sc "babel" "plugin-transform-named-capturing-groups-regex") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-named-capturing-groups-regex/-/plugin-transform-named-capturing-groups-regex-7.16.8.tgz") "7f860e0e40d844a02c9dcf9d84965e7dfd666252" [
          (s."@babel/helper-create-regexp-features-plugin@^7.16.7")
          ];
        "@babel/plugin-transform-named-capturing-groups-regex@^7.16.8" = s."@babel/plugin-transform-named-capturing-groups-regex@7.16.8";
        "@babel/plugin-transform-new-target@7.16.7" = f (sc "babel" "plugin-transform-new-target") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-new-target/-/plugin-transform-new-target-7.16.7.tgz") "9967d89a5c243818e0800fdad89db22c5f514244" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-new-target@^7.16.7" = s."@babel/plugin-transform-new-target@7.16.7";
        "@babel/plugin-transform-object-super@7.16.7" = f (sc "babel" "plugin-transform-object-super") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-object-super/-/plugin-transform-object-super-7.16.7.tgz") "ac359cf8d32cf4354d27a46867999490b6c32a94" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-replace-supers@^7.16.7")
          ];
        "@babel/plugin-transform-object-super@^7.16.7" = s."@babel/plugin-transform-object-super@7.16.7";
        "@babel/plugin-transform-parameters@7.16.7" = f (sc "babel" "plugin-transform-parameters") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-parameters/-/plugin-transform-parameters-7.16.7.tgz") "a1721f55b99b736511cb7e0152f61f17688f331f" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-parameters@^7.12.1" = s."@babel/plugin-transform-parameters@7.16.7";
        "@babel/plugin-transform-parameters@^7.16.7" = s."@babel/plugin-transform-parameters@7.16.7";
        "@babel/plugin-transform-property-literals@7.16.7" = f (sc "babel" "plugin-transform-property-literals") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-property-literals/-/plugin-transform-property-literals-7.16.7.tgz") "2dadac85155436f22c696c4827730e0fe1057a55" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-property-literals@^7.16.7" = s."@babel/plugin-transform-property-literals@7.16.7";
        "@babel/plugin-transform-react-display-name@7.16.7" = f (sc "babel" "plugin-transform-react-display-name") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-react-display-name/-/plugin-transform-react-display-name-7.16.7.tgz") "7b6d40d232f4c0f550ea348593db3b21e2404340" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-react-display-name@^7.16.7" = s."@babel/plugin-transform-react-display-name@7.16.7";
        "@babel/plugin-transform-react-jsx-development@7.16.7" = f (sc "babel" "plugin-transform-react-jsx-development") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-react-jsx-development/-/plugin-transform-react-jsx-development-7.16.7.tgz") "43a00724a3ed2557ed3f276a01a929e6686ac7b8" [
          (s."@babel/plugin-transform-react-jsx@^7.16.7")
          ];
        "@babel/plugin-transform-react-jsx-development@^7.16.7" = s."@babel/plugin-transform-react-jsx-development@7.16.7";
        "@babel/plugin-transform-react-jsx@7.16.7" = f (sc "babel" "plugin-transform-react-jsx") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-react-jsx/-/plugin-transform-react-jsx-7.16.7.tgz") "86a6a220552afd0e4e1f0388a68a372be7add0d4" [
          (s."@babel/helper-annotate-as-pure@^7.16.7")
          (s."@babel/helper-module-imports@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-jsx@^7.16.7")
          (s."@babel/types@^7.16.7")
          ];
        "@babel/plugin-transform-react-jsx@^7.12.12" = s."@babel/plugin-transform-react-jsx@7.16.7";
        "@babel/plugin-transform-react-jsx@^7.16.7" = s."@babel/plugin-transform-react-jsx@7.16.7";
        "@babel/plugin-transform-react-pure-annotations@7.16.7" = f (sc "babel" "plugin-transform-react-pure-annotations") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-react-pure-annotations/-/plugin-transform-react-pure-annotations-7.16.7.tgz") "232bfd2f12eb551d6d7d01d13fe3f86b45eb9c67" [
          (s."@babel/helper-annotate-as-pure@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-react-pure-annotations@^7.16.7" = s."@babel/plugin-transform-react-pure-annotations@7.16.7";
        "@babel/plugin-transform-regenerator@7.16.7" = f (sc "babel" "plugin-transform-regenerator") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-regenerator/-/plugin-transform-regenerator-7.16.7.tgz") "9e7576dc476cb89ccc5096fff7af659243b4adeb" [
          (s."regenerator-transform@^0.14.2")
          ];
        "@babel/plugin-transform-regenerator@^7.16.7" = s."@babel/plugin-transform-regenerator@7.16.7";
        "@babel/plugin-transform-reserved-words@7.16.7" = f (sc "babel" "plugin-transform-reserved-words") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-reserved-words/-/plugin-transform-reserved-words-7.16.7.tgz") "1d798e078f7c5958eec952059c460b220a63f586" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-reserved-words@^7.16.7" = s."@babel/plugin-transform-reserved-words@7.16.7";
        "@babel/plugin-transform-shorthand-properties@7.16.7" = f (sc "babel" "plugin-transform-shorthand-properties") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-shorthand-properties/-/plugin-transform-shorthand-properties-7.16.7.tgz") "e8549ae4afcf8382f711794c0c7b6b934c5fbd2a" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-shorthand-properties@^7.12.1" = s."@babel/plugin-transform-shorthand-properties@7.16.7";
        "@babel/plugin-transform-shorthand-properties@^7.16.7" = s."@babel/plugin-transform-shorthand-properties@7.16.7";
        "@babel/plugin-transform-spread@7.16.7" = f (sc "babel" "plugin-transform-spread") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-spread/-/plugin-transform-spread-7.16.7.tgz") "a303e2122f9f12e0105daeedd0f30fb197d8ff44" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-skip-transparent-expression-wrappers@^7.16.0")
          ];
        "@babel/plugin-transform-spread@^7.12.1" = s."@babel/plugin-transform-spread@7.16.7";
        "@babel/plugin-transform-spread@^7.16.7" = s."@babel/plugin-transform-spread@7.16.7";
        "@babel/plugin-transform-sticky-regex@7.16.7" = f (sc "babel" "plugin-transform-sticky-regex") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-sticky-regex/-/plugin-transform-sticky-regex-7.16.7.tgz") "c84741d4f4a38072b9a1e2e3fd56d359552e8660" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-sticky-regex@^7.16.7" = s."@babel/plugin-transform-sticky-regex@7.16.7";
        "@babel/plugin-transform-template-literals@7.16.7" = f (sc "babel" "plugin-transform-template-literals") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-template-literals/-/plugin-transform-template-literals-7.16.7.tgz") "f3d1c45d28967c8e80f53666fc9c3e50618217ab" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-template-literals@^7.12.1" = s."@babel/plugin-transform-template-literals@7.16.7";
        "@babel/plugin-transform-template-literals@^7.16.7" = s."@babel/plugin-transform-template-literals@7.16.7";
        "@babel/plugin-transform-typeof-symbol@7.16.7" = f (sc "babel" "plugin-transform-typeof-symbol") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-typeof-symbol/-/plugin-transform-typeof-symbol-7.16.7.tgz") "9cdbe622582c21368bd482b660ba87d5545d4f7e" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-typeof-symbol@^7.16.7" = s."@babel/plugin-transform-typeof-symbol@7.16.7";
        "@babel/plugin-transform-typescript@7.16.8" = f (sc "babel" "plugin-transform-typescript") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-typescript/-/plugin-transform-typescript-7.16.8.tgz") "591ce9b6b83504903fa9dd3652c357c2ba7a1ee0" [
          (s."@babel/helper-create-class-features-plugin@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/plugin-syntax-typescript@^7.16.7")
          ];
        "@babel/plugin-transform-typescript@^7.16.7" = s."@babel/plugin-transform-typescript@7.16.8";
        "@babel/plugin-transform-unicode-escapes@7.16.7" = f (sc "babel" "plugin-transform-unicode-escapes") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-unicode-escapes/-/plugin-transform-unicode-escapes-7.16.7.tgz") "da8717de7b3287a2c6d659750c964f302b31ece3" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-unicode-escapes@^7.16.7" = s."@babel/plugin-transform-unicode-escapes@7.16.7";
        "@babel/plugin-transform-unicode-regex@7.16.7" = f (sc "babel" "plugin-transform-unicode-regex") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/plugin-transform-unicode-regex/-/plugin-transform-unicode-regex-7.16.7.tgz") "0f7aa4a501198976e25e82702574c34cfebe9ef2" [
          (s."@babel/helper-create-regexp-features-plugin@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          ];
        "@babel/plugin-transform-unicode-regex@^7.16.7" = s."@babel/plugin-transform-unicode-regex@7.16.7";
        "@babel/preset-env@7.16.11" = f (sc "babel" "preset-env") "7.16.11" (ir "https://registry.yarnpkg.com/@babel/preset-env/-/preset-env-7.16.11.tgz") "5dd88fd885fae36f88fd7c8342475c9f0abe2982" [
          (s."@babel/compat-data@^7.16.8")
          (s."@babel/helper-compilation-targets@^7.16.7")
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-validator-option@^7.16.7")
          (s."@babel/plugin-bugfix-safari-id-destructuring-collision-in-function-expression@^7.16.7")
          (s."@babel/plugin-bugfix-v8-spread-parameters-in-optional-chaining@^7.16.7")
          (s."@babel/plugin-proposal-async-generator-functions@^7.16.8")
          (s."@babel/plugin-proposal-class-properties@^7.16.7")
          (s."@babel/plugin-proposal-class-static-block@^7.16.7")
          (s."@babel/plugin-proposal-dynamic-import@^7.16.7")
          (s."@babel/plugin-proposal-export-namespace-from@^7.16.7")
          (s."@babel/plugin-proposal-json-strings@^7.16.7")
          (s."@babel/plugin-proposal-logical-assignment-operators@^7.16.7")
          (s."@babel/plugin-proposal-nullish-coalescing-operator@^7.16.7")
          (s."@babel/plugin-proposal-numeric-separator@^7.16.7")
          (s."@babel/plugin-proposal-object-rest-spread@^7.16.7")
          (s."@babel/plugin-proposal-optional-catch-binding@^7.16.7")
          (s."@babel/plugin-proposal-optional-chaining@^7.16.7")
          (s."@babel/plugin-proposal-private-methods@^7.16.11")
          (s."@babel/plugin-proposal-private-property-in-object@^7.16.7")
          (s."@babel/plugin-proposal-unicode-property-regex@^7.16.7")
          (s."@babel/plugin-syntax-async-generators@^7.8.4")
          (s."@babel/plugin-syntax-class-properties@^7.12.13")
          (s."@babel/plugin-syntax-class-static-block@^7.14.5")
          (s."@babel/plugin-syntax-dynamic-import@^7.8.3")
          (s."@babel/plugin-syntax-export-namespace-from@^7.8.3")
          (s."@babel/plugin-syntax-json-strings@^7.8.3")
          (s."@babel/plugin-syntax-logical-assignment-operators@^7.10.4")
          (s."@babel/plugin-syntax-nullish-coalescing-operator@^7.8.3")
          (s."@babel/plugin-syntax-numeric-separator@^7.10.4")
          (s."@babel/plugin-syntax-object-rest-spread@^7.8.3")
          (s."@babel/plugin-syntax-optional-catch-binding@^7.8.3")
          (s."@babel/plugin-syntax-optional-chaining@^7.8.3")
          (s."@babel/plugin-syntax-private-property-in-object@^7.14.5")
          (s."@babel/plugin-syntax-top-level-await@^7.14.5")
          (s."@babel/plugin-transform-arrow-functions@^7.16.7")
          (s."@babel/plugin-transform-async-to-generator@^7.16.8")
          (s."@babel/plugin-transform-block-scoped-functions@^7.16.7")
          (s."@babel/plugin-transform-block-scoping@^7.16.7")
          (s."@babel/plugin-transform-classes@^7.16.7")
          (s."@babel/plugin-transform-computed-properties@^7.16.7")
          (s."@babel/plugin-transform-destructuring@^7.16.7")
          (s."@babel/plugin-transform-dotall-regex@^7.16.7")
          (s."@babel/plugin-transform-duplicate-keys@^7.16.7")
          (s."@babel/plugin-transform-exponentiation-operator@^7.16.7")
          (s."@babel/plugin-transform-for-of@^7.16.7")
          (s."@babel/plugin-transform-function-name@^7.16.7")
          (s."@babel/plugin-transform-literals@^7.16.7")
          (s."@babel/plugin-transform-member-expression-literals@^7.16.7")
          (s."@babel/plugin-transform-modules-amd@^7.16.7")
          (s."@babel/plugin-transform-modules-commonjs@^7.16.8")
          (s."@babel/plugin-transform-modules-systemjs@^7.16.7")
          (s."@babel/plugin-transform-modules-umd@^7.16.7")
          (s."@babel/plugin-transform-named-capturing-groups-regex@^7.16.8")
          (s."@babel/plugin-transform-new-target@^7.16.7")
          (s."@babel/plugin-transform-object-super@^7.16.7")
          (s."@babel/plugin-transform-parameters@^7.16.7")
          (s."@babel/plugin-transform-property-literals@^7.16.7")
          (s."@babel/plugin-transform-regenerator@^7.16.7")
          (s."@babel/plugin-transform-reserved-words@^7.16.7")
          (s."@babel/plugin-transform-shorthand-properties@^7.16.7")
          (s."@babel/plugin-transform-spread@^7.16.7")
          (s."@babel/plugin-transform-sticky-regex@^7.16.7")
          (s."@babel/plugin-transform-template-literals@^7.16.7")
          (s."@babel/plugin-transform-typeof-symbol@^7.16.7")
          (s."@babel/plugin-transform-unicode-escapes@^7.16.7")
          (s."@babel/plugin-transform-unicode-regex@^7.16.7")
          (s."@babel/preset-modules@^0.1.5")
          (s."@babel/types@^7.16.8")
          (s."babel-plugin-polyfill-corejs2@^0.3.0")
          (s."babel-plugin-polyfill-corejs3@^0.5.0")
          (s."babel-plugin-polyfill-regenerator@^0.3.0")
          (s."core-js-compat@^3.20.2")
          (s."semver@^6.3.0")
          ];
        "@babel/preset-env@^7.12.11" = s."@babel/preset-env@7.16.11";
        "@babel/preset-flow@7.16.7" = f (sc "babel" "preset-flow") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/preset-flow/-/preset-flow-7.16.7.tgz") "7fd831323ab25eeba6e4b77a589f680e30581cbd" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-validator-option@^7.16.7")
          (s."@babel/plugin-transform-flow-strip-types@^7.16.7")
          ];
        "@babel/preset-flow@^7.12.1" = s."@babel/preset-flow@7.16.7";
        "@babel/preset-modules@0.1.5" = f (sc "babel" "preset-modules") "0.1.5" (ir "https://registry.yarnpkg.com/@babel/preset-modules/-/preset-modules-0.1.5.tgz") "ef939d6e7f268827e1841638dc6ff95515e115d9" [
          (s."@babel/helper-plugin-utils@^7.0.0")
          (s."@babel/plugin-proposal-unicode-property-regex@^7.4.4")
          (s."@babel/plugin-transform-dotall-regex@^7.4.4")
          (s."@babel/types@^7.4.4")
          (s."esutils@^2.0.2")
          ];
        "@babel/preset-modules@^0.1.5" = s."@babel/preset-modules@0.1.5";
        "@babel/preset-react@7.16.7" = f (sc "babel" "preset-react") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/preset-react/-/preset-react-7.16.7.tgz") "4c18150491edc69c183ff818f9f2aecbe5d93852" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-validator-option@^7.16.7")
          (s."@babel/plugin-transform-react-display-name@^7.16.7")
          (s."@babel/plugin-transform-react-jsx@^7.16.7")
          (s."@babel/plugin-transform-react-jsx-development@^7.16.7")
          (s."@babel/plugin-transform-react-pure-annotations@^7.16.7")
          ];
        "@babel/preset-react@^7.12.10" = s."@babel/preset-react@7.16.7";
        "@babel/preset-typescript@7.16.7" = f (sc "babel" "preset-typescript") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/preset-typescript/-/preset-typescript-7.16.7.tgz") "ab114d68bb2020afc069cd51b37ff98a046a70b9" [
          (s."@babel/helper-plugin-utils@^7.16.7")
          (s."@babel/helper-validator-option@^7.16.7")
          (s."@babel/plugin-transform-typescript@^7.16.7")
          ];
        "@babel/preset-typescript@^7.12.7" = s."@babel/preset-typescript@7.16.7";
        "@babel/register@7.16.9" = f (sc "babel" "register") "7.16.9" (ir "https://registry.yarnpkg.com/@babel/register/-/register-7.16.9.tgz") "fcfb23cfdd9ad95c9771e58183de83b513857806" [
          (s."clone-deep@^4.0.1")
          (s."find-cache-dir@^2.0.0")
          (s."make-dir@^2.1.0")
          (s."pirates@^4.0.0")
          (s."source-map-support@^0.5.16")
          ];
        "@babel/register@^7.12.1" = s."@babel/register@7.16.9";
        "@babel/runtime@7.16.7" = f (sc "babel" "runtime") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/runtime/-/runtime-7.16.7.tgz") "03ff99f64106588c9c403c6ecb8c3bafbbdff1fa" [
          (s."regenerator-runtime@^0.13.4")
          ];
        "@babel/runtime@^7.0.0" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.10.2" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.12.5" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.14.8" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.16.7" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.3.1" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.5.0" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.5.5" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.7.2" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.7.6" = s."@babel/runtime@7.16.7";
        "@babel/runtime@^7.8.4" = s."@babel/runtime@7.16.7";
        "@babel/template@7.16.7" = f (sc "babel" "template") "7.16.7" (ir "https://registry.yarnpkg.com/@babel/template/-/template-7.16.7.tgz") "8d126c8701fde4d66b264b3eba3d96f07666d155" [
          (s."@babel/code-frame@^7.16.7")
          (s."@babel/parser@^7.16.7")
          (s."@babel/types@^7.16.7")
          ];
        "@babel/template@^7.12.7" = s."@babel/template@7.16.7";
        "@babel/template@^7.16.7" = s."@babel/template@7.16.7";
        "@babel/traverse@7.16.10" = f (sc "babel" "traverse") "7.16.10" (ir "https://registry.yarnpkg.com/@babel/traverse/-/traverse-7.16.10.tgz") "448f940defbe95b5a8029975b051f75993e8239f" [
          (s."@babel/code-frame@^7.16.7")
          (s."@babel/generator@^7.16.8")
          (s."@babel/helper-environment-visitor@^7.16.7")
          (s."@babel/helper-function-name@^7.16.7")
          (s."@babel/helper-hoist-variables@^7.16.7")
          (s."@babel/helper-split-export-declaration@^7.16.7")
          (s."@babel/parser@^7.16.10")
          (s."@babel/types@^7.16.8")
          (s."debug@^4.1.0")
          (s."globals@^11.1.0")
          ];
        "@babel/traverse@^7.1.6" = s."@babel/traverse@7.16.10";
        "@babel/traverse@^7.12.11" = s."@babel/traverse@7.16.10";
        "@babel/traverse@^7.12.9" = s."@babel/traverse@7.16.10";
        "@babel/traverse@^7.13.0" = s."@babel/traverse@7.16.10";
        "@babel/traverse@^7.16.10" = s."@babel/traverse@7.16.10";
        "@babel/traverse@^7.16.7" = s."@babel/traverse@7.16.10";
        "@babel/traverse@^7.16.8" = s."@babel/traverse@7.16.10";
        "@babel/types@7.16.8" = f (sc "babel" "types") "7.16.8" (ir "https://registry.yarnpkg.com/@babel/types/-/types-7.16.8.tgz") "0ba5da91dd71e0a4e7781a30f22770831062e3c1" [
          (s."@babel/helper-validator-identifier@^7.16.7")
          (s."to-fast-properties@^2.0.0")
          ];
        "@babel/types@^7.12.11" = s."@babel/types@7.16.8";
        "@babel/types@^7.12.7" = s."@babel/types@7.16.8";
        "@babel/types@^7.16.0" = s."@babel/types@7.16.8";
        "@babel/types@^7.16.7" = s."@babel/types@7.16.8";
        "@babel/types@^7.16.8" = s."@babel/types@7.16.8";
        "@babel/types@^7.2.0" = s."@babel/types@7.16.8";
        "@babel/types@^7.4.4" = s."@babel/types@7.16.8";
        "@base2/pretty-print-object@1.0.1" = f (sc "base2" "pretty-print-object") "1.0.1" (ir "https://registry.yarnpkg.com/@base2/pretty-print-object/-/pretty-print-object-1.0.1.tgz") "371ba8be66d556812dc7fb169ebc3c08378f69d4" [];
        "@bcoe/v8-coverage@0.2.3" = f (sc "bcoe" "v8-coverage") "0.2.3" (ir "https://registry.yarnpkg.com/@bcoe/v8-coverage/-/v8-coverage-0.2.3.tgz") "75a2e8b51cb758a7553d6804a5932d7aace75c39" [];
        "@bcoe/v8-coverage@^0.2.3" = s."@bcoe/v8-coverage@0.2.3";
        "@cnakazawa/watch@1.0.4" = f (sc "cnakazawa" "watch") "1.0.4" (ir "https://registry.yarnpkg.com/@cnakazawa/watch/-/watch-1.0.4.tgz") "f864ae85004d0fcab6f50be9141c4da368d1656a" [
          (s."exec-sh@^0.3.2")
          (s."minimist@^1.2.0")
          ];
        "@cnakazawa/watch@^1.0.3" = s."@cnakazawa/watch@1.0.4";
        "@discoveryjs/json-ext@0.5.6" = f (sc "discoveryjs" "json-ext") "0.5.6" (ir "https://registry.yarnpkg.com/@discoveryjs/json-ext/-/json-ext-0.5.6.tgz") "d5e0706cf8c6acd8c6032f8d54070af261bbbb2f" [];
        "@discoveryjs/json-ext@^0.5.3" = s."@discoveryjs/json-ext@0.5.6";
        "@emotion/cache@10.0.29" = f (sc "emotion" "cache") "10.0.29" (ir "https://registry.yarnpkg.com/@emotion/cache/-/cache-10.0.29.tgz") "87e7e64f412c060102d589fe7c6dc042e6f9d1e0" [
          (s."@emotion/sheet@0.9.4")
          (s."@emotion/stylis@0.8.5")
          (s."@emotion/utils@0.11.3")
          (s."@emotion/weak-memoize@0.2.5")
          ];
        "@emotion/cache@^10.0.27" = s."@emotion/cache@10.0.29";
        "@emotion/core@10.3.1" = f (sc "emotion" "core") "10.3.1" (ir "https://registry.yarnpkg.com/@emotion/core/-/core-10.3.1.tgz") "4021b6d8b33b3304d48b0bb478485e7d7421c69d" [
          (s."@babel/runtime@^7.5.5")
          (s."@emotion/cache@^10.0.27")
          (s."@emotion/css@^10.0.27")
          (s."@emotion/serialize@^0.11.15")
          (s."@emotion/sheet@0.9.4")
          (s."@emotion/utils@0.11.3")
          ];
        "@emotion/core@^10.1.1" = s."@emotion/core@10.3.1";
        "@emotion/css@10.0.27" = f (sc "emotion" "css") "10.0.27" (ir "https://registry.yarnpkg.com/@emotion/css/-/css-10.0.27.tgz") "3a7458198fbbebb53b01b2b87f64e5e21241e14c" [
          (s."@emotion/serialize@^0.11.15")
          (s."@emotion/utils@0.11.3")
          (s."babel-plugin-emotion@^10.0.27")
          ];
        "@emotion/css@^10.0.27" = s."@emotion/css@10.0.27";
        "@emotion/hash@0.8.0" = f (sc "emotion" "hash") "0.8.0" (ir "https://registry.yarnpkg.com/@emotion/hash/-/hash-0.8.0.tgz") "bbbff68978fefdbe68ccb533bc8cbe1d1afb5413" [];
        "@emotion/is-prop-valid@0.8.8" = f (sc "emotion" "is-prop-valid") "0.8.8" (ir "https://registry.yarnpkg.com/@emotion/is-prop-valid/-/is-prop-valid-0.8.8.tgz") "db28b1c4368a259b60a97311d6a952d4fd01ac1a" [
          (s."@emotion/memoize@0.7.4")
          ];
        "@emotion/is-prop-valid@^0.8.6" = s."@emotion/is-prop-valid@0.8.8";
        "@emotion/memoize@0.7.4" = f (sc "emotion" "memoize") "0.7.4" (ir "https://registry.yarnpkg.com/@emotion/memoize/-/memoize-0.7.4.tgz") "19bf0f5af19149111c40d98bb0cf82119f5d9eeb" [];
        "@emotion/serialize@0.11.16" = f (sc "emotion" "serialize") "0.11.16" (ir "https://registry.yarnpkg.com/@emotion/serialize/-/serialize-0.11.16.tgz") "dee05f9e96ad2fb25a5206b6d759b2d1ed3379ad" [
          (s."@emotion/hash@0.8.0")
          (s."@emotion/memoize@0.7.4")
          (s."@emotion/unitless@0.7.5")
          (s."@emotion/utils@0.11.3")
          (s."csstype@^2.5.7")
          ];
        "@emotion/serialize@^0.11.15" = s."@emotion/serialize@0.11.16";
        "@emotion/serialize@^0.11.16" = s."@emotion/serialize@0.11.16";
        "@emotion/sheet@0.9.4" = f (sc "emotion" "sheet") "0.9.4" (ir "https://registry.yarnpkg.com/@emotion/sheet/-/sheet-0.9.4.tgz") "894374bea39ec30f489bbfc3438192b9774d32e5" [];
        "@emotion/styled-base@10.3.0" = f (sc "emotion" "styled-base") "10.3.0" (ir "https://registry.yarnpkg.com/@emotion/styled-base/-/styled-base-10.3.0.tgz") "9aa2c946100f78b47316e4bc6048321afa6d4e36" [
          (s."@babel/runtime@^7.5.5")
          (s."@emotion/is-prop-valid@0.8.8")
          (s."@emotion/serialize@^0.11.15")
          (s."@emotion/utils@0.11.3")
          ];
        "@emotion/styled-base@^10.3.0" = s."@emotion/styled-base@10.3.0";
        "@emotion/styled@10.3.0" = f (sc "emotion" "styled") "10.3.0" (ir "https://registry.yarnpkg.com/@emotion/styled/-/styled-10.3.0.tgz") "8ee959bf75730789abb5f67f7c3ded0c30aec876" [
          (s."@emotion/styled-base@^10.3.0")
          (s."babel-plugin-emotion@^10.0.27")
          ];
        "@emotion/styled@^10.0.27" = s."@emotion/styled@10.3.0";
        "@emotion/stylis@0.8.5" = f (sc "emotion" "stylis") "0.8.5" (ir "https://registry.yarnpkg.com/@emotion/stylis/-/stylis-0.8.5.tgz") "deacb389bd6ee77d1e7fcaccce9e16c5c7e78e04" [];
        "@emotion/unitless@0.7.5" = f (sc "emotion" "unitless") "0.7.5" (ir "https://registry.yarnpkg.com/@emotion/unitless/-/unitless-0.7.5.tgz") "77211291c1900a700b8a78cfafda3160d76949ed" [];
        "@emotion/utils@0.11.3" = f (sc "emotion" "utils") "0.11.3" (ir "https://registry.yarnpkg.com/@emotion/utils/-/utils-0.11.3.tgz") "a759863867befa7e583400d322652a3f44820924" [];
        "@emotion/weak-memoize@0.2.5" = f (sc "emotion" "weak-memoize") "0.2.5" (ir "https://registry.yarnpkg.com/@emotion/weak-memoize/-/weak-memoize-0.2.5.tgz") "8eed982e2ee6f7f4e44c253e12962980791efd46" [];
        "@gar/promisify@1.1.2" = f (sc "gar" "promisify") "1.1.2" (ir "https://registry.yarnpkg.com/@gar/promisify/-/promisify-1.1.2.tgz") "30aa825f11d438671d585bd44e7fd564535fc210" [];
        "@gar/promisify@^1.0.1" = s."@gar/promisify@1.1.2";
        "@istanbuljs/load-nyc-config@1.1.0" = f (sc "istanbuljs" "load-nyc-config") "1.1.0" (ir "https://registry.yarnpkg.com/@istanbuljs/load-nyc-config/-/load-nyc-config-1.1.0.tgz") "fd3db1d59ecf7cf121e80650bb86712f9b55eced" [
          (s."camelcase@^5.3.1")
          (s."find-up@^4.1.0")
          (s."get-package-type@^0.1.0")
          (s."js-yaml@^3.13.1")
          (s."resolve-from@^5.0.0")
          ];
        "@istanbuljs/load-nyc-config@^1.0.0" = s."@istanbuljs/load-nyc-config@1.1.0";
        "@istanbuljs/schema@0.1.3" = f (sc "istanbuljs" "schema") "0.1.3" (ir "https://registry.yarnpkg.com/@istanbuljs/schema/-/schema-0.1.3.tgz") "e45e384e4b8ec16bce2fd903af78450f6bf7ec98" [];
        "@istanbuljs/schema@^0.1.2" = s."@istanbuljs/schema@0.1.3";
        "@jest/transform@26.6.2" = f (sc "jest" "transform") "26.6.2" (ir "https://registry.yarnpkg.com/@jest/transform/-/transform-26.6.2.tgz") "5ac57c5fa1ad17b2aae83e73e45813894dcf2e4b" [
          (s."@babel/core@^7.1.0")
          (s."@jest/types@^26.6.2")
          (s."babel-plugin-istanbul@^6.0.0")
          (s."chalk@^4.0.0")
          (s."convert-source-map@^1.4.0")
          (s."fast-json-stable-stringify@^2.0.0")
          (s."graceful-fs@^4.2.4")
          (s."jest-haste-map@^26.6.2")
          (s."jest-regex-util@^26.0.0")
          (s."jest-util@^26.6.2")
          (s."micromatch@^4.0.2")
          (s."pirates@^4.0.1")
          (s."slash@^3.0.0")
          (s."source-map@^0.6.1")
          (s."write-file-atomic@^3.0.0")
          ];
        "@jest/transform@^26.6.2" = s."@jest/transform@26.6.2";
        "@jest/types@26.6.2" = f (sc "jest" "types") "26.6.2" (ir "https://registry.yarnpkg.com/@jest/types/-/types-26.6.2.tgz") "bef5a532030e1d88a2f5a6d933f84e97226ed48e" [
          (s."@types/istanbul-lib-coverage@^2.0.0")
          (s."@types/istanbul-reports@^3.0.0")
          (s."@types/node@*")
          (s."@types/yargs@^15.0.0")
          (s."chalk@^4.0.0")
          ];
        "@jest/types@^26.6.2" = s."@jest/types@26.6.2";
        "@mdx-js/loader@1.6.22" = f (sc "mdx-js" "loader") "1.6.22" (ir "https://registry.yarnpkg.com/@mdx-js/loader/-/loader-1.6.22.tgz") "d9e8fe7f8185ff13c9c8639c048b123e30d322c4" [
          (s."@mdx-js/mdx@1.6.22")
          (s."@mdx-js/react@1.6.22")
          (s."loader-utils@2.0.0")
          ];
        "@mdx-js/loader@^1.6.22" = s."@mdx-js/loader@1.6.22";
        "@mdx-js/mdx@1.6.22" = f (sc "mdx-js" "mdx") "1.6.22" (ir "https://registry.yarnpkg.com/@mdx-js/mdx/-/mdx-1.6.22.tgz") "8a723157bf90e78f17dc0f27995398e6c731f1ba" [
          (s."@babel/core@7.12.9")
          (s."@babel/plugin-syntax-jsx@7.12.1")
          (s."@babel/plugin-syntax-object-rest-spread@7.8.3")
          (s."@mdx-js/util@1.6.22")
          (s."babel-plugin-apply-mdx-type-prop@1.6.22")
          (s."babel-plugin-extract-import-names@1.6.22")
          (s."camelcase-css@2.0.1")
          (s."detab@2.0.4")
          (s."hast-util-raw@6.0.1")
          (s."lodash.uniq@4.5.0")
          (s."mdast-util-to-hast@10.0.1")
          (s."remark-footnotes@2.0.0")
          (s."remark-mdx@1.6.22")
          (s."remark-parse@8.0.3")
          (s."remark-squeeze-paragraphs@4.0.0")
          (s."style-to-object@0.3.0")
          (s."unified@9.2.0")
          (s."unist-builder@2.0.3")
          (s."unist-util-visit@2.0.3")
          ];
        "@mdx-js/mdx@^1.6.22" = s."@mdx-js/mdx@1.6.22";
        "@mdx-js/react@1.6.22" = f (sc "mdx-js" "react") "1.6.22" (ir "https://registry.yarnpkg.com/@mdx-js/react/-/react-1.6.22.tgz") "ae09b4744fddc74714ee9f9d6f17a66e77c43573" [];
        "@mdx-js/react@^1.6.22" = s."@mdx-js/react@1.6.22";
        "@mdx-js/util@1.6.22" = f (sc "mdx-js" "util") "1.6.22" (ir "https://registry.yarnpkg.com/@mdx-js/util/-/util-1.6.22.tgz") "219dfd89ae5b97a8801f015323ffa4b62f45718b" [];
        "@mrmlnc/readdir-enhanced@2.2.1" = f (sc "mrmlnc" "readdir-enhanced") "2.2.1" (ir "https://registry.yarnpkg.com/@mrmlnc/readdir-enhanced/-/readdir-enhanced-2.2.1.tgz") "524af240d1a360527b730475ecfa1344aa540dde" [
          (s."call-me-maybe@^1.0.1")
          (s."glob-to-regexp@^0.3.0")
          ];
        "@mrmlnc/readdir-enhanced@^2.2.1" = s."@mrmlnc/readdir-enhanced@2.2.1";
        "@nodelib/fs.scandir@2.1.5" = f (sc "nodelib" "fs.scandir") "2.1.5" (ir "https://registry.yarnpkg.com/@nodelib/fs.scandir/-/fs.scandir-2.1.5.tgz") "7619c2eb21b25483f6d167548b4cfd5a7488c3d5" [
          (s."@nodelib/fs.stat@2.0.5")
          (s."run-parallel@^1.1.9")
          ];
        "@nodelib/fs.stat@1.1.3" = f (sc "nodelib" "fs.stat") "1.1.3" (ir "https://registry.yarnpkg.com/@nodelib/fs.stat/-/fs.stat-1.1.3.tgz") "2b5a3ab3f918cca48a8c754c08168e3f03eba61b" [];
        "@nodelib/fs.stat@2.0.5" = f (sc "nodelib" "fs.stat") "2.0.5" (ir "https://registry.yarnpkg.com/@nodelib/fs.stat/-/fs.stat-2.0.5.tgz") "5bd262af94e9d25bd1e71b05deed44876a222e8b" [];
        "@nodelib/fs.stat@^1.1.2" = s."@nodelib/fs.stat@1.1.3";
        "@nodelib/fs.stat@^2.0.2" = s."@nodelib/fs.stat@2.0.5";
        "@nodelib/fs.walk@1.2.8" = f (sc "nodelib" "fs.walk") "1.2.8" (ir "https://registry.yarnpkg.com/@nodelib/fs.walk/-/fs.walk-1.2.8.tgz") "e95737e8bb6746ddedf69c556953494f196fe69a" [
          (s."@nodelib/fs.scandir@2.1.5")
          (s."fastq@^1.6.0")
          ];
        "@nodelib/fs.walk@^1.2.3" = s."@nodelib/fs.walk@1.2.8";
        "@npmcli/fs@1.1.0" = f (sc "npmcli" "fs") "1.1.0" (ir "https://registry.yarnpkg.com/@npmcli/fs/-/fs-1.1.0.tgz") "bec1d1b89c170d40e1b73ad6c943b0b75e7d2951" [
          (s."@gar/promisify@^1.0.1")
          (s."semver@^7.3.5")
          ];
        "@npmcli/fs@^1.0.0" = s."@npmcli/fs@1.1.0";
        "@npmcli/move-file@1.1.2" = f (sc "npmcli" "move-file") "1.1.2" (ir "https://registry.yarnpkg.com/@npmcli/move-file/-/move-file-1.1.2.tgz") "1a82c3e372f7cae9253eb66d72543d6b8685c674" [
          (s."mkdirp@^1.0.4")
          (s."rimraf@^3.0.2")
          ];
        "@npmcli/move-file@^1.0.1" = s."@npmcli/move-file@1.1.2";
        "@pmmmwh/react-refresh-webpack-plugin@0.5.4" = f (sc "pmmmwh" "react-refresh-webpack-plugin") "0.5.4" (ir "https://registry.yarnpkg.com/@pmmmwh/react-refresh-webpack-plugin/-/react-refresh-webpack-plugin-0.5.4.tgz") "df0d0d855fc527db48aac93c218a0bf4ada41f99" [
          (s."ansi-html-community@^0.0.8")
          (s."common-path-prefix@^3.0.0")
          (s."core-js-pure@^3.8.1")
          (s."error-stack-parser@^2.0.6")
          (s."find-up@^5.0.0")
          (s."html-entities@^2.1.0")
          (s."loader-utils@^2.0.0")
          (s."schema-utils@^3.0.0")
          (s."source-map@^0.7.3")
          ];
        "@pmmmwh/react-refresh-webpack-plugin@^0.5.1" = s."@pmmmwh/react-refresh-webpack-plugin@0.5.4";
        "@popperjs/core@2.11.2" = f (sc "popperjs" "core") "2.11.2" (ir "https://registry.yarnpkg.com/@popperjs/core/-/core-2.11.2.tgz") "830beaec4b4091a9e9398ac50f865ddea52186b9" [];
        "@popperjs/core@^2.5.4" = s."@popperjs/core@2.11.2";
        "@popperjs/core@^2.6.0" = s."@popperjs/core@2.11.2";
        "@storybook/addon-actions@6.4.14" = f (sc "storybook" "addon-actions") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-actions/-/addon-actions-6.4.14.tgz") "be5590438943487b4243b2fe16619557cb9ca0ce" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/theming@6.4.14")
          (s."core-js@^3.8.2")
          (s."fast-deep-equal@^3.1.3")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."polished@^4.0.5")
          (s."prop-types@^15.7.2")
          (s."react-inspector@^5.1.0")
          (s."regenerator-runtime@^0.13.7")
          (s."telejson@^5.3.2")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          (s."uuid-browser@^3.1.0")
          ];
        "@storybook/addon-actions@^6.4.14" = s."@storybook/addon-actions@6.4.14";
        "@storybook/addon-backgrounds@6.4.14" = f (sc "storybook" "addon-backgrounds") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-backgrounds/-/addon-backgrounds-6.4.14.tgz") "6e55963857993f6a55a01dc8df99c60224bb86af" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/theming@6.4.14")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."memoizerific@^1.11.3")
          (s."regenerator-runtime@^0.13.7")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/addon-controls@6.4.14" = f (sc "storybook" "addon-controls") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-controls/-/addon-controls-6.4.14.tgz") "05a2d58175bf5af00d408386f18bcb2f62da431b" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core-common@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/node-logger@6.4.14")
          (s."@storybook/store@6.4.14")
          (s."@storybook/theming@6.4.14")
          (s."core-js@^3.8.2")
          (s."lodash@^4.17.21")
          (s."ts-dedent@^2.0.0")
          ];
        "@storybook/addon-docs@6.4.14" = f (sc "storybook" "addon-docs") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-docs/-/addon-docs-6.4.14.tgz") "0be1faf6d0b4deae93e37a838a2aae197cead74a" [
          (s."@babel/core@^7.12.10")
          (s."@babel/generator@^7.12.11")
          (s."@babel/parser@^7.12.11")
          (s."@babel/plugin-transform-react-jsx@^7.12.12")
          (s."@babel/preset-env@^7.12.11")
          (s."@jest/transform@^26.6.2")
          (s."@mdx-js/loader@^1.6.22")
          (s."@mdx-js/mdx@^1.6.22")
          (s."@mdx-js/react@^1.6.22")
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/builder-webpack4@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/csf-tools@6.4.14")
          (s."@storybook/node-logger@6.4.14")
          (s."@storybook/postinstall@6.4.14")
          (s."@storybook/preview-web@6.4.14")
          (s."@storybook/source-loader@6.4.14")
          (s."@storybook/store@6.4.14")
          (s."@storybook/theming@6.4.14")
          (s."acorn@^7.4.1")
          (s."acorn-jsx@^5.3.1")
          (s."acorn-walk@^7.2.0")
          (s."core-js@^3.8.2")
          (s."doctrine@^3.0.0")
          (s."escodegen@^2.0.0")
          (s."fast-deep-equal@^3.1.3")
          (s."global@^4.4.0")
          (s."html-tags@^3.1.0")
          (s."js-string-escape@^1.0.1")
          (s."loader-utils@^2.0.0")
          (s."lodash@^4.17.21")
          (s."nanoid@^3.1.23")
          (s."p-limit@^3.1.0")
          (s."prettier@>=2.2.1 <=2.3.0")
          (s."prop-types@^15.7.2")
          (s."react-element-to-jsx-string@^14.3.4")
          (s."regenerator-runtime@^0.13.7")
          (s."remark-external-links@^8.0.0")
          (s."remark-slug@^6.0.0")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/addon-essentials@6.4.14" = f (sc "storybook" "addon-essentials") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-essentials/-/addon-essentials-6.4.14.tgz") "25eea7f148d7decac59565631afa3c755631c169" [
          (s."@storybook/addon-actions@6.4.14")
          (s."@storybook/addon-backgrounds@6.4.14")
          (s."@storybook/addon-controls@6.4.14")
          (s."@storybook/addon-docs@6.4.14")
          (s."@storybook/addon-measure@6.4.14")
          (s."@storybook/addon-outline@6.4.14")
          (s."@storybook/addon-toolbars@6.4.14")
          (s."@storybook/addon-viewport@6.4.14")
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/node-logger@6.4.14")
          (s."core-js@^3.8.2")
          (s."regenerator-runtime@^0.13.7")
          (s."ts-dedent@^2.0.0")
          ];
        "@storybook/addon-essentials@^6.4.14" = s."@storybook/addon-essentials@6.4.14";
        "@storybook/addon-links@6.4.14" = f (sc "storybook" "addon-links") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-links/-/addon-links-6.4.14.tgz") "7dedd5502cf1fecdcfc845e59a10769cb32ec175" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/router@6.4.14")
          (s."@types/qs@^6.9.5")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."prop-types@^15.7.2")
          (s."qs@^6.10.0")
          (s."regenerator-runtime@^0.13.7")
          (s."ts-dedent@^2.0.0")
          ];
        "@storybook/addon-links@^6.4.14" = s."@storybook/addon-links@6.4.14";
        "@storybook/addon-measure@6.4.14" = f (sc "storybook" "addon-measure") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-measure/-/addon-measure-6.4.14.tgz") "5bd35b12e09f82b0f3dad2e477e26014d5c016e6" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          ];
        "@storybook/addon-outline@6.4.14" = f (sc "storybook" "addon-outline") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-outline/-/addon-outline-6.4.14.tgz") "a75b29b961717710a1d14e5bf1628affec329053" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."regenerator-runtime@^0.13.7")
          (s."ts-dedent@^2.0.0")
          ];
        "@storybook/addon-toolbars@6.4.14" = f (sc "storybook" "addon-toolbars") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-toolbars/-/addon-toolbars-6.4.14.tgz") "cfb9b900919c1179747018d02604359d85d3988d" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/theming@6.4.14")
          (s."core-js@^3.8.2")
          (s."regenerator-runtime@^0.13.7")
          ];
        "@storybook/addon-viewport@6.4.14" = f (sc "storybook" "addon-viewport") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addon-viewport/-/addon-viewport-6.4.14.tgz") "7e0988f9e8348712a334d1fa3df8fe6134d5d9ed" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/theming@6.4.14")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."memoizerific@^1.11.3")
          (s."prop-types@^15.7.2")
          (s."regenerator-runtime@^0.13.7")
          ];
        "@storybook/addons@6.4.14" = f (sc "storybook" "addons") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/addons/-/addons-6.4.14.tgz") "45d6937bc2ece33ceadc5358b2a2298d2a0d1e95" [
          (s."@storybook/api@6.4.14")
          (s."@storybook/channels@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/router@6.4.14")
          (s."@storybook/theming@6.4.14")
          (s."@types/webpack-env@^1.16.0")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."regenerator-runtime@^0.13.7")
          ];
        "@storybook/api@6.4.14" = f (sc "storybook" "api") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/api/-/api-6.4.14.tgz") "a477646f7e020a362f044d2e614e3d1a86ba8f6f" [
          (s."@storybook/channels@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/router@6.4.14")
          (s."@storybook/semver@^7.3.2")
          (s."@storybook/theming@6.4.14")
          (s."core-js@^3.8.2")
          (s."fast-deep-equal@^3.1.3")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."memoizerific@^1.11.3")
          (s."regenerator-runtime@^0.13.7")
          (s."store2@^2.12.0")
          (s."telejson@^5.3.2")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/builder-webpack4@6.4.14" = f (sc "storybook" "builder-webpack4") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/builder-webpack4/-/builder-webpack4-6.4.14.tgz") "77f45d164f5b93776fa154252706c6b73bd0edc5" [
          (s."@babel/core@^7.12.10")
          (s."@babel/plugin-proposal-class-properties@^7.12.1")
          (s."@babel/plugin-proposal-decorators@^7.12.12")
          (s."@babel/plugin-proposal-export-default-from@^7.12.1")
          (s."@babel/plugin-proposal-nullish-coalescing-operator@^7.12.1")
          (s."@babel/plugin-proposal-object-rest-spread@^7.12.1")
          (s."@babel/plugin-proposal-optional-chaining@^7.12.7")
          (s."@babel/plugin-proposal-private-methods@^7.12.1")
          (s."@babel/plugin-syntax-dynamic-import@^7.8.3")
          (s."@babel/plugin-transform-arrow-functions@^7.12.1")
          (s."@babel/plugin-transform-block-scoping@^7.12.12")
          (s."@babel/plugin-transform-classes@^7.12.1")
          (s."@babel/plugin-transform-destructuring@^7.12.1")
          (s."@babel/plugin-transform-for-of@^7.12.1")
          (s."@babel/plugin-transform-parameters@^7.12.1")
          (s."@babel/plugin-transform-shorthand-properties@^7.12.1")
          (s."@babel/plugin-transform-spread@^7.12.1")
          (s."@babel/plugin-transform-template-literals@^7.12.1")
          (s."@babel/preset-env@^7.12.11")
          (s."@babel/preset-react@^7.12.10")
          (s."@babel/preset-typescript@^7.12.7")
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/channel-postmessage@6.4.14")
          (s."@storybook/channels@6.4.14")
          (s."@storybook/client-api@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core-common@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/node-logger@6.4.14")
          (s."@storybook/preview-web@6.4.14")
          (s."@storybook/router@6.4.14")
          (s."@storybook/semver@^7.3.2")
          (s."@storybook/store@6.4.14")
          (s."@storybook/theming@6.4.14")
          (s."@storybook/ui@6.4.14")
          (s."@types/node@^14.0.10")
          (s."@types/webpack@^4.41.26")
          (s."autoprefixer@^9.8.6")
          (s."babel-loader@^8.0.0")
          (s."babel-plugin-macros@^2.8.0")
          (s."babel-plugin-polyfill-corejs3@^0.1.0")
          (s."case-sensitive-paths-webpack-plugin@^2.3.0")
          (s."core-js@^3.8.2")
          (s."css-loader@^3.6.0")
          (s."file-loader@^6.2.0")
          (s."find-up@^5.0.0")
          (s."fork-ts-checker-webpack-plugin@^4.1.6")
          (s."glob@^7.1.6")
          (s."glob-promise@^3.4.0")
          (s."global@^4.4.0")
          (s."html-webpack-plugin@^4.0.0")
          (s."pnp-webpack-plugin@1.6.4")
          (s."postcss@^7.0.36")
          (s."postcss-flexbugs-fixes@^4.2.1")
          (s."postcss-loader@^4.2.0")
          (s."raw-loader@^4.0.2")
          (s."stable@^0.1.8")
          (s."style-loader@^1.3.0")
          (s."terser-webpack-plugin@^4.2.3")
          (s."ts-dedent@^2.0.0")
          (s."url-loader@^4.1.1")
          (s."util-deprecate@^1.0.2")
          (s."webpack@4")
          (s."webpack-dev-middleware@^3.7.3")
          (s."webpack-filter-warnings-plugin@^1.2.1")
          (s."webpack-hot-middleware@^2.25.1")
          (s."webpack-virtual-modules@^0.2.2")
          ];
        "@storybook/channel-postmessage@6.4.14" = f (sc "storybook" "channel-postmessage") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/channel-postmessage/-/channel-postmessage-6.4.14.tgz") "ce719041768ea8c0d64b7edc32ec7c774fba9b19" [
          (s."@storybook/channels@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."qs@^6.10.0")
          (s."telejson@^5.3.2")
          ];
        "@storybook/channel-websocket@6.4.14" = f (sc "storybook" "channel-websocket") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/channel-websocket/-/channel-websocket-6.4.14.tgz") "d71a4c8a4b36e2e89a4a3c56af3e0aa50353e02f" [
          (s."@storybook/channels@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."telejson@^5.3.2")
          ];
        "@storybook/channels@6.4.14" = f (sc "storybook" "channels") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/channels/-/channels-6.4.14.tgz") "f7a5416c971febd26ed7b03a75d99fd819790e48" [
          (s."core-js@^3.8.2")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/client-api@6.4.14" = f (sc "storybook" "client-api") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/client-api/-/client-api-6.4.14.tgz") "d2053511971e06d70bba2accfbd1f6c0f2084e2a" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/channel-postmessage@6.4.14")
          (s."@storybook/channels@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/store@6.4.14")
          (s."@types/qs@^6.9.5")
          (s."@types/webpack-env@^1.16.0")
          (s."core-js@^3.8.2")
          (s."fast-deep-equal@^3.1.3")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."memoizerific@^1.11.3")
          (s."qs@^6.10.0")
          (s."regenerator-runtime@^0.13.7")
          (s."store2@^2.12.0")
          (s."synchronous-promise@^2.0.15")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/client-logger@6.4.14" = f (sc "storybook" "client-logger") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/client-logger/-/client-logger-6.4.14.tgz") "a7aed982407e4146548f9ac4b3af5eba24cd045e" [
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          ];
        "@storybook/components@6.4.14" = f (sc "storybook" "components") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/components/-/components-6.4.14.tgz") "546b34fe3feb09e670b76ff71d889bf5f566f1e4" [
          (s."@popperjs/core@^2.6.0")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/theming@6.4.14")
          (s."@types/color-convert@^2.0.0")
          (s."@types/overlayscrollbars@^1.12.0")
          (s."@types/react-syntax-highlighter@11.0.5")
          (s."color-convert@^2.0.1")
          (s."core-js@^3.8.2")
          (s."fast-deep-equal@^3.1.3")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."markdown-to-jsx@^7.1.3")
          (s."memoizerific@^1.11.3")
          (s."overlayscrollbars@^1.13.1")
          (s."polished@^4.0.5")
          (s."prop-types@^15.7.2")
          (s."react-colorful@^5.1.2")
          (s."react-popper-tooltip@^3.1.1")
          (s."react-syntax-highlighter@^13.5.3")
          (s."react-textarea-autosize@^8.3.0")
          (s."regenerator-runtime@^0.13.7")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/core-client@6.4.14" = f (sc "storybook" "core-client") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/core-client/-/core-client-6.4.14.tgz") "8f5dc6fe2295e479225bd396404a43679a15637e" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/channel-postmessage@6.4.14")
          (s."@storybook/channel-websocket@6.4.14")
          (s."@storybook/client-api@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/preview-web@6.4.14")
          (s."@storybook/store@6.4.14")
          (s."@storybook/ui@6.4.14")
          (s."airbnb-js-shims@^2.2.1")
          (s."ansi-to-html@^0.6.11")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."qs@^6.10.0")
          (s."regenerator-runtime@^0.13.7")
          (s."ts-dedent@^2.0.0")
          (s."unfetch@^4.2.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/core-common@6.4.14" = f (sc "storybook" "core-common") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/core-common/-/core-common-6.4.14.tgz") "137b935855f0cc785ec55b386312747949e30e99" [
          (s."@babel/core@^7.12.10")
          (s."@babel/plugin-proposal-class-properties@^7.12.1")
          (s."@babel/plugin-proposal-decorators@^7.12.12")
          (s."@babel/plugin-proposal-export-default-from@^7.12.1")
          (s."@babel/plugin-proposal-nullish-coalescing-operator@^7.12.1")
          (s."@babel/plugin-proposal-object-rest-spread@^7.12.1")
          (s."@babel/plugin-proposal-optional-chaining@^7.12.7")
          (s."@babel/plugin-proposal-private-methods@^7.12.1")
          (s."@babel/plugin-syntax-dynamic-import@^7.8.3")
          (s."@babel/plugin-transform-arrow-functions@^7.12.1")
          (s."@babel/plugin-transform-block-scoping@^7.12.12")
          (s."@babel/plugin-transform-classes@^7.12.1")
          (s."@babel/plugin-transform-destructuring@^7.12.1")
          (s."@babel/plugin-transform-for-of@^7.12.1")
          (s."@babel/plugin-transform-parameters@^7.12.1")
          (s."@babel/plugin-transform-shorthand-properties@^7.12.1")
          (s."@babel/plugin-transform-spread@^7.12.1")
          (s."@babel/preset-env@^7.12.11")
          (s."@babel/preset-react@^7.12.10")
          (s."@babel/preset-typescript@^7.12.7")
          (s."@babel/register@^7.12.1")
          (s."@storybook/node-logger@6.4.14")
          (s."@storybook/semver@^7.3.2")
          (s."@types/node@^14.0.10")
          (s."@types/pretty-hrtime@^1.0.0")
          (s."babel-loader@^8.0.0")
          (s."babel-plugin-macros@^3.0.1")
          (s."babel-plugin-polyfill-corejs3@^0.1.0")
          (s."chalk@^4.1.0")
          (s."core-js@^3.8.2")
          (s."express@^4.17.1")
          (s."file-system-cache@^1.0.5")
          (s."find-up@^5.0.0")
          (s."fork-ts-checker-webpack-plugin@^6.0.4")
          (s."fs-extra@^9.0.1")
          (s."glob@^7.1.6")
          (s."handlebars@^4.7.7")
          (s."interpret@^2.2.0")
          (s."json5@^2.1.3")
          (s."lazy-universal-dotenv@^3.0.1")
          (s."picomatch@^2.3.0")
          (s."pkg-dir@^5.0.0")
          (s."pretty-hrtime@^1.0.3")
          (s."resolve-from@^5.0.0")
          (s."slash@^3.0.0")
          (s."telejson@^5.3.2")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          (s."webpack@4")
          ];
        "@storybook/core-events@6.4.14" = f (sc "storybook" "core-events") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/core-events/-/core-events-6.4.14.tgz") "37293c0fce703f2643cec6f24fc6ef7c40e30ded" [
          (s."core-js@^3.8.2")
          ];
        "@storybook/core-server@6.4.14" = f (sc "storybook" "core-server") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/core-server/-/core-server-6.4.14.tgz") "0bef36e1203eb56e1c9bbf7f02122500c8f7d534" [
          (s."@discoveryjs/json-ext@^0.5.3")
          (s."@storybook/builder-webpack4@6.4.14")
          (s."@storybook/core-client@6.4.14")
          (s."@storybook/core-common@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/csf-tools@6.4.14")
          (s."@storybook/manager-webpack4@6.4.14")
          (s."@storybook/node-logger@6.4.14")
          (s."@storybook/semver@^7.3.2")
          (s."@storybook/store@6.4.14")
          (s."@types/node@^14.0.10")
          (s."@types/node-fetch@^2.5.7")
          (s."@types/pretty-hrtime@^1.0.0")
          (s."@types/webpack@^4.41.26")
          (s."better-opn@^2.1.1")
          (s."boxen@^5.1.2")
          (s."chalk@^4.1.0")
          (s."cli-table3@^0.6.1")
          (s."commander@^6.2.1")
          (s."compression@^1.7.4")
          (s."core-js@^3.8.2")
          (s."cpy@^8.1.2")
          (s."detect-port@^1.3.0")
          (s."express@^4.17.1")
          (s."file-system-cache@^1.0.5")
          (s."fs-extra@^9.0.1")
          (s."globby@^11.0.2")
          (s."ip@^1.1.5")
          (s."lodash@^4.17.21")
          (s."node-fetch@^2.6.1")
          (s."pretty-hrtime@^1.0.3")
          (s."prompts@^2.4.0")
          (s."regenerator-runtime@^0.13.7")
          (s."serve-favicon@^2.5.0")
          (s."slash@^3.0.0")
          (s."telejson@^5.3.3")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          (s."watchpack@^2.2.0")
          (s."webpack@4")
          (s."ws@^8.2.3")
          ];
        "@storybook/core@6.4.14" = f (sc "storybook" "core") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/core/-/core-6.4.14.tgz") "c20a1432f22603eb2d3523389ff1311fffbba24f" [
          (s."@storybook/core-client@6.4.14")
          (s."@storybook/core-server@6.4.14")
          ];
        "@storybook/csf-tools@6.4.14" = f (sc "storybook" "csf-tools") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/csf-tools/-/csf-tools-6.4.14.tgz") "c5112e17f07dae4c7b922aefd45dccbbc9e49803" [
          (s."@babel/core@^7.12.10")
          (s."@babel/generator@^7.12.11")
          (s."@babel/parser@^7.12.11")
          (s."@babel/plugin-transform-react-jsx@^7.12.12")
          (s."@babel/preset-env@^7.12.11")
          (s."@babel/traverse@^7.12.11")
          (s."@babel/types@^7.12.11")
          (s."@mdx-js/mdx@^1.6.22")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."core-js@^3.8.2")
          (s."fs-extra@^9.0.1")
          (s."global@^4.4.0")
          (s."js-string-escape@^1.0.1")
          (s."lodash@^4.17.21")
          (s."prettier@>=2.2.1 <=2.3.0")
          (s."regenerator-runtime@^0.13.7")
          (s."ts-dedent@^2.0.0")
          ];
        "@storybook/csf@0.0.2--canary.87bc651.0" = f (sc "storybook" "csf") "0.0.2--canary.87bc651.0" (ir "https://registry.yarnpkg.com/@storybook/csf/-/csf-0.0.2--canary.87bc651.0.tgz") "c7b99b3a344117ef67b10137b6477a3d2750cf44" [
          (s."lodash@^4.17.15")
          ];
        "@storybook/manager-webpack4@6.4.14" = f (sc "storybook" "manager-webpack4") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/manager-webpack4/-/manager-webpack4-6.4.14.tgz") "b8ca3a11d0fb18ef6ca3e58e1c36b2eb8226ccbf" [
          (s."@babel/core@^7.12.10")
          (s."@babel/plugin-transform-template-literals@^7.12.1")
          (s."@babel/preset-react@^7.12.10")
          (s."@storybook/addons@6.4.14")
          (s."@storybook/core-client@6.4.14")
          (s."@storybook/core-common@6.4.14")
          (s."@storybook/node-logger@6.4.14")
          (s."@storybook/theming@6.4.14")
          (s."@storybook/ui@6.4.14")
          (s."@types/node@^14.0.10")
          (s."@types/webpack@^4.41.26")
          (s."babel-loader@^8.0.0")
          (s."case-sensitive-paths-webpack-plugin@^2.3.0")
          (s."chalk@^4.1.0")
          (s."core-js@^3.8.2")
          (s."css-loader@^3.6.0")
          (s."express@^4.17.1")
          (s."file-loader@^6.2.0")
          (s."file-system-cache@^1.0.5")
          (s."find-up@^5.0.0")
          (s."fs-extra@^9.0.1")
          (s."html-webpack-plugin@^4.0.0")
          (s."node-fetch@^2.6.1")
          (s."pnp-webpack-plugin@1.6.4")
          (s."read-pkg-up@^7.0.1")
          (s."regenerator-runtime@^0.13.7")
          (s."resolve-from@^5.0.0")
          (s."style-loader@^1.3.0")
          (s."telejson@^5.3.2")
          (s."terser-webpack-plugin@^4.2.3")
          (s."ts-dedent@^2.0.0")
          (s."url-loader@^4.1.1")
          (s."util-deprecate@^1.0.2")
          (s."webpack@4")
          (s."webpack-dev-middleware@^3.7.3")
          (s."webpack-virtual-modules@^0.2.2")
          ];
        "@storybook/node-logger@6.4.14" = f (sc "storybook" "node-logger") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/node-logger/-/node-logger-6.4.14.tgz") "2e96f4e3e06c78c3d065e59818515209122d9ae4" [
          (s."@types/npmlog@^4.1.2")
          (s."chalk@^4.1.0")
          (s."core-js@^3.8.2")
          (s."npmlog@^5.0.1")
          (s."pretty-hrtime@^1.0.3")
          ];
        "@storybook/postinstall@6.4.14" = f (sc "storybook" "postinstall") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/postinstall/-/postinstall-6.4.14.tgz") "e8f7925529d4955783660f409deee1e907897b2b" [
          (s."core-js@^3.8.2")
          ];
        "@storybook/preview-web@6.4.14" = f (sc "storybook" "preview-web") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/preview-web/-/preview-web-6.4.14.tgz") "4d7035d5aa0e8c41c9a2ff21c2a3b3cbae9f3688" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/channel-postmessage@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/store@6.4.14")
          (s."ansi-to-html@^0.6.11")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."qs@^6.10.0")
          (s."regenerator-runtime@^0.13.7")
          (s."synchronous-promise@^2.0.15")
          (s."ts-dedent@^2.0.0")
          (s."unfetch@^4.2.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/react-docgen-typescript-plugin@1.0.2-canary.253f8c1.0" = f (sc "storybook" "react-docgen-typescript-plugin") "1.0.2-canary.253f8c1.0" (ir "https://registry.yarnpkg.com/@storybook/react-docgen-typescript-plugin/-/react-docgen-typescript-plugin-1.0.2-canary.253f8c1.0.tgz") "f2da40e6aae4aa586c2fb284a4a1744602c3c7fa" [
          (s."debug@^4.1.1")
          (s."endent@^2.0.1")
          (s."find-cache-dir@^3.3.1")
          (s."flat-cache@^3.0.4")
          (s."micromatch@^4.0.2")
          (s."react-docgen-typescript@^2.0.0")
          (s."tslib@^2.0.0")
          ];
        "@storybook/react@6.4.14" = f (sc "storybook" "react") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/react/-/react-6.4.14.tgz") "7be241ecfa412312681bb54c82327764d70ffb70" [
          (s."@babel/preset-flow@^7.12.1")
          (s."@babel/preset-react@^7.12.10")
          (s."@pmmmwh/react-refresh-webpack-plugin@^0.5.1")
          (s."@storybook/addons@6.4.14")
          (s."@storybook/core@6.4.14")
          (s."@storybook/core-common@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."@storybook/node-logger@6.4.14")
          (s."@storybook/react-docgen-typescript-plugin@1.0.2-canary.253f8c1.0")
          (s."@storybook/semver@^7.3.2")
          (s."@storybook/store@6.4.14")
          (s."@types/webpack-env@^1.16.0")
          (s."babel-plugin-add-react-displayname@^0.0.5")
          (s."babel-plugin-named-asset-import@^0.3.1")
          (s."babel-plugin-react-docgen@^4.2.1")
          (s."core-js@^3.8.2")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."prop-types@^15.7.2")
          (s."react-refresh@^0.11.0")
          (s."read-pkg-up@^7.0.1")
          (s."regenerator-runtime@^0.13.7")
          (s."ts-dedent@^2.0.0")
          (s."webpack@4")
          ];
        "@storybook/react@^6.4.14" = s."@storybook/react@6.4.14";
        "@storybook/router@6.4.14" = f (sc "storybook" "router") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/router/-/router-6.4.14.tgz") "46fd46eadafc0d6b647be13702704c5fcf8f11e3" [
          (s."@storybook/client-logger@6.4.14")
          (s."core-js@^3.8.2")
          (s."fast-deep-equal@^3.1.3")
          (s."global@^4.4.0")
          (s."history@5.0.0")
          (s."lodash@^4.17.21")
          (s."memoizerific@^1.11.3")
          (s."qs@^6.10.0")
          (s."react-router@^6.0.0")
          (s."react-router-dom@^6.0.0")
          (s."ts-dedent@^2.0.0")
          ];
        "@storybook/semver@7.3.2" = f (sc "storybook" "semver") "7.3.2" (ir "https://registry.yarnpkg.com/@storybook/semver/-/semver-7.3.2.tgz") "f3b9c44a1c9a0b933c04e66d0048fcf2fa10dac0" [
          (s."core-js@^3.6.5")
          (s."find-up@^4.1.0")
          ];
        "@storybook/semver@^7.3.2" = s."@storybook/semver@7.3.2";
        "@storybook/source-loader@6.4.14" = f (sc "storybook" "source-loader") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/source-loader/-/source-loader-6.4.14.tgz") "d1e6c2df0918e4867b3e4b5ce748685938f77d87" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."core-js@^3.8.2")
          (s."estraverse@^5.2.0")
          (s."global@^4.4.0")
          (s."loader-utils@^2.0.0")
          (s."lodash@^4.17.21")
          (s."prettier@>=2.2.1 <=2.3.0")
          (s."regenerator-runtime@^0.13.7")
          ];
        "@storybook/store@6.4.14" = f (sc "storybook" "store") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/store/-/store-6.4.14.tgz") "2ec5601e1c40a27f164b570d4c2b84c57d3a4115" [
          (s."@storybook/addons@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/csf@0.0.2--canary.87bc651.0")
          (s."core-js@^3.8.2")
          (s."fast-deep-equal@^3.1.3")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."memoizerific@^1.11.3")
          (s."regenerator-runtime@^0.13.7")
          (s."slash@^3.0.0")
          (s."stable@^0.1.8")
          (s."synchronous-promise@^2.0.15")
          (s."ts-dedent@^2.0.0")
          (s."util-deprecate@^1.0.2")
          ];
        "@storybook/theming@6.4.14" = f (sc "storybook" "theming") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/theming/-/theming-6.4.14.tgz") "f034914eb1853a80f588c7c141d47af0595f6d1f" [
          (s."@emotion/core@^10.1.1")
          (s."@emotion/is-prop-valid@^0.8.6")
          (s."@emotion/styled@^10.0.27")
          (s."@storybook/client-logger@6.4.14")
          (s."core-js@^3.8.2")
          (s."deep-object-diff@^1.1.0")
          (s."emotion-theming@^10.0.27")
          (s."global@^4.4.0")
          (s."memoizerific@^1.11.3")
          (s."polished@^4.0.5")
          (s."resolve-from@^5.0.0")
          (s."ts-dedent@^2.0.0")
          ];
        "@storybook/ui@6.4.14" = f (sc "storybook" "ui") "6.4.14" (ir "https://registry.yarnpkg.com/@storybook/ui/-/ui-6.4.14.tgz") "59f08ac8d8eb782fa13fc9a8dd715222c96bf234" [
          (s."@emotion/core@^10.1.1")
          (s."@storybook/addons@6.4.14")
          (s."@storybook/api@6.4.14")
          (s."@storybook/channels@6.4.14")
          (s."@storybook/client-logger@6.4.14")
          (s."@storybook/components@6.4.14")
          (s."@storybook/core-events@6.4.14")
          (s."@storybook/router@6.4.14")
          (s."@storybook/semver@^7.3.2")
          (s."@storybook/theming@6.4.14")
          (s."copy-to-clipboard@^3.3.1")
          (s."core-js@^3.8.2")
          (s."core-js-pure@^3.8.2")
          (s."downshift@^6.0.15")
          (s."emotion-theming@^10.0.27")
          (s."fuse.js@^3.6.1")
          (s."global@^4.4.0")
          (s."lodash@^4.17.21")
          (s."markdown-to-jsx@^7.1.3")
          (s."memoizerific@^1.11.3")
          (s."polished@^4.0.5")
          (s."qs@^6.10.0")
          (s."react-draggable@^4.4.3")
          (s."react-helmet-async@^1.0.7")
          (s."react-sizeme@^3.0.1")
          (s."regenerator-runtime@^0.13.7")
          (s."resolve-from@^5.0.0")
          (s."store2@^2.12.0")
          ];
        "@types/color-convert@2.0.0" = f (sc "types" "color-convert") "2.0.0" (ir "https://registry.yarnpkg.com/@types/color-convert/-/color-convert-2.0.0.tgz") "8f5ee6b9e863dcbee5703f5a517ffb13d3ea4e22" [
          (s."@types/color-name@*")
          ];
        "@types/color-convert@^2.0.0" = s."@types/color-convert@2.0.0";
        "@types/color-name@*" = s."@types/color-name@1.1.1";
        "@types/color-name@1.1.1" = f (sc "types" "color-name") "1.1.1" (ir "https://registry.yarnpkg.com/@types/color-name/-/color-name-1.1.1.tgz") "1c1261bbeaa10a8055bbc5d8ab84b7b2afc846a0" [];
        "@types/glob@*" = s."@types/glob@7.2.0";
        "@types/glob@7.2.0" = f (sc "types" "glob") "7.2.0" (ir "https://registry.yarnpkg.com/@types/glob/-/glob-7.2.0.tgz") "bc1b5bf3aa92f25bd5dd39f35c57361bdce5b2eb" [
          (s."@types/minimatch@*")
          (s."@types/node@*")
          ];
        "@types/glob@^7.1.1" = s."@types/glob@7.2.0";
        "@types/graceful-fs@4.1.5" = f (sc "types" "graceful-fs") "4.1.5" (ir "https://registry.yarnpkg.com/@types/graceful-fs/-/graceful-fs-4.1.5.tgz") "21ffba0d98da4350db64891f92a9e5db3cdb4e15" [
          (s."@types/node@*")
          ];
        "@types/graceful-fs@^4.1.2" = s."@types/graceful-fs@4.1.5";
        "@types/hast@2.3.4" = f (sc "types" "hast") "2.3.4" (ir "https://registry.yarnpkg.com/@types/hast/-/hast-2.3.4.tgz") "8aa5ef92c117d20d974a82bdfb6a648b08c0bafc" [
          (s."@types/unist@*")
          ];
        "@types/hast@^2.0.0" = s."@types/hast@2.3.4";
        "@types/html-minifier-terser@5.1.2" = f (sc "types" "html-minifier-terser") "5.1.2" (ir "https://registry.yarnpkg.com/@types/html-minifier-terser/-/html-minifier-terser-5.1.2.tgz") "693b316ad323ea97eed6b38ed1a3cc02b1672b57" [];
        "@types/html-minifier-terser@^5.0.0" = s."@types/html-minifier-terser@5.1.2";
        "@types/is-function@1.0.1" = f (sc "types" "is-function") "1.0.1" (ir "https://registry.yarnpkg.com/@types/is-function/-/is-function-1.0.1.tgz") "2d024eace950c836d9e3335a66b97960ae41d022" [];
        "@types/is-function@^1.0.0" = s."@types/is-function@1.0.1";
        "@types/istanbul-lib-coverage@*" = s."@types/istanbul-lib-coverage@2.0.4";
        "@types/istanbul-lib-coverage@2.0.4" = f (sc "types" "istanbul-lib-coverage") "2.0.4" (ir "https://registry.yarnpkg.com/@types/istanbul-lib-coverage/-/istanbul-lib-coverage-2.0.4.tgz") "8467d4b3c087805d63580480890791277ce35c44" [];
        "@types/istanbul-lib-coverage@^2.0.0" = s."@types/istanbul-lib-coverage@2.0.4";
        "@types/istanbul-lib-coverage@^2.0.1" = s."@types/istanbul-lib-coverage@2.0.4";
        "@types/istanbul-lib-report@*" = s."@types/istanbul-lib-report@3.0.0";
        "@types/istanbul-lib-report@3.0.0" = f (sc "types" "istanbul-lib-report") "3.0.0" (ir "https://registry.yarnpkg.com/@types/istanbul-lib-report/-/istanbul-lib-report-3.0.0.tgz") "c14c24f18ea8190c118ee7562b7ff99a36552686" [
          (s."@types/istanbul-lib-coverage@*")
          ];
        "@types/istanbul-reports@3.0.1" = f (sc "types" "istanbul-reports") "3.0.1" (ir "https://registry.yarnpkg.com/@types/istanbul-reports/-/istanbul-reports-3.0.1.tgz") "9153fe98bba2bd565a63add9436d6f0d7f8468ff" [
          (s."@types/istanbul-lib-report@*")
          ];
        "@types/istanbul-reports@^3.0.0" = s."@types/istanbul-reports@3.0.1";
        "@types/json-schema@7.0.9" = f (sc "types" "json-schema") "7.0.9" (ir "https://registry.yarnpkg.com/@types/json-schema/-/json-schema-7.0.9.tgz") "97edc9037ea0c38585320b28964dde3b39e4660d" [];
        "@types/json-schema@^7.0.4" = s."@types/json-schema@7.0.9";
        "@types/json-schema@^7.0.5" = s."@types/json-schema@7.0.9";
        "@types/json-schema@^7.0.8" = s."@types/json-schema@7.0.9";
        "@types/mdast@3.0.10" = f (sc "types" "mdast") "3.0.10" (ir "https://registry.yarnpkg.com/@types/mdast/-/mdast-3.0.10.tgz") "4724244a82a4598884cbbe9bcfd73dff927ee8af" [
          (s."@types/unist@*")
          ];
        "@types/mdast@^3.0.0" = s."@types/mdast@3.0.10";
        "@types/minimatch@*" = s."@types/minimatch@3.0.5";
        "@types/minimatch@3.0.5" = f (sc "types" "minimatch") "3.0.5" (ir "https://registry.yarnpkg.com/@types/minimatch/-/minimatch-3.0.5.tgz") "1001cc5e6a3704b83c236027e77f2f58ea010f40" [];
        "@types/node-fetch@2.5.12" = f (sc "types" "node-fetch") "2.5.12" (ir "https://registry.yarnpkg.com/@types/node-fetch/-/node-fetch-2.5.12.tgz") "8a6f779b1d4e60b7a57fb6fd48d84fb545b9cc66" [
          (s."@types/node@*")
          (s."form-data@^3.0.0")
          ];
        "@types/node-fetch@^2.5.7" = s."@types/node-fetch@2.5.12";
        "@types/node@*" = s."@types/node@17.0.12";
        "@types/node@14.18.9" = f (sc "types" "node") "14.18.9" (ir "https://registry.yarnpkg.com/@types/node/-/node-14.18.9.tgz") "0e5944eefe2b287391279a19b407aa98bd14436d" [];
        "@types/node@17.0.12" = f (sc "types" "node") "17.0.12" (ir "https://registry.yarnpkg.com/@types/node/-/node-17.0.12.tgz") "f7aa331b27f08244888c47b7df126184bc2339c5" [];
        "@types/node@^14.0.10" = s."@types/node@14.18.9";
        "@types/normalize-package-data@2.4.1" = f (sc "types" "normalize-package-data") "2.4.1" (ir "https://registry.yarnpkg.com/@types/normalize-package-data/-/normalize-package-data-2.4.1.tgz") "d3357479a0fdfdd5907fe67e17e0a85c906e1301" [];
        "@types/normalize-package-data@^2.4.0" = s."@types/normalize-package-data@2.4.1";
        "@types/npmlog@4.1.4" = f (sc "types" "npmlog") "4.1.4" (ir "https://registry.yarnpkg.com/@types/npmlog/-/npmlog-4.1.4.tgz") "30eb872153c7ead3e8688c476054ddca004115f6" [];
        "@types/npmlog@^4.1.2" = s."@types/npmlog@4.1.4";
        "@types/overlayscrollbars@1.12.1" = f (sc "types" "overlayscrollbars") "1.12.1" (ir "https://registry.yarnpkg.com/@types/overlayscrollbars/-/overlayscrollbars-1.12.1.tgz") "fb637071b545834fb12aea94ee309a2ff4cdc0a8" [];
        "@types/overlayscrollbars@^1.12.0" = s."@types/overlayscrollbars@1.12.1";
        "@types/parse-json@4.0.0" = f (sc "types" "parse-json") "4.0.0" (ir "https://registry.yarnpkg.com/@types/parse-json/-/parse-json-4.0.0.tgz") "2f8bb441434d163b35fb8ffdccd7138927ffb8c0" [];
        "@types/parse-json@^4.0.0" = s."@types/parse-json@4.0.0";
        "@types/parse5@5.0.3" = f (sc "types" "parse5") "5.0.3" (ir "https://registry.yarnpkg.com/@types/parse5/-/parse5-5.0.3.tgz") "e7b5aebbac150f8b5fdd4a46e7f0bd8e65e19109" [];
        "@types/parse5@^5.0.0" = s."@types/parse5@5.0.3";
        "@types/pretty-hrtime@1.0.1" = f (sc "types" "pretty-hrtime") "1.0.1" (ir "https://registry.yarnpkg.com/@types/pretty-hrtime/-/pretty-hrtime-1.0.1.tgz") "72a26101dc567b0d68fd956cf42314556e42d601" [];
        "@types/pretty-hrtime@^1.0.0" = s."@types/pretty-hrtime@1.0.1";
        "@types/prop-types@*" = s."@types/prop-types@15.7.4";
        "@types/prop-types@15.7.4" = f (sc "types" "prop-types") "15.7.4" (ir "https://registry.yarnpkg.com/@types/prop-types/-/prop-types-15.7.4.tgz") "fcf7205c25dff795ee79af1e30da2c9790808f11" [];
        "@types/qs@6.9.7" = f (sc "types" "qs") "6.9.7" (ir "https://registry.yarnpkg.com/@types/qs/-/qs-6.9.7.tgz") "63bb7d067db107cc1e457c303bc25d511febf6cb" [];
        "@types/qs@^6.9.5" = s."@types/qs@6.9.7";
        "@types/react-syntax-highlighter@11.0.5" = f (sc "types" "react-syntax-highlighter") "11.0.5" (ir "https://registry.yarnpkg.com/@types/react-syntax-highlighter/-/react-syntax-highlighter-11.0.5.tgz") "0d546261b4021e1f9d85b50401c0a42acb106087" [
          (s."@types/react@*")
          ];
        "@types/react@*" = s."@types/react@17.0.38";
        "@types/react@17.0.38" = f (sc "types" "react") "17.0.38" (ir "https://registry.yarnpkg.com/@types/react/-/react-17.0.38.tgz") "f24249fefd89357d5fa71f739a686b8d7c7202bd" [
          (s."@types/prop-types@*")
          (s."@types/scheduler@*")
          (s."csstype@^3.0.2")
          ];
        "@types/scheduler@*" = s."@types/scheduler@0.16.2";
        "@types/scheduler@0.16.2" = f (sc "types" "scheduler") "0.16.2" (ir "https://registry.yarnpkg.com/@types/scheduler/-/scheduler-0.16.2.tgz") "1a62f89525723dde24ba1b01b092bf5df8ad4d39" [];
        "@types/source-list-map@*" = s."@types/source-list-map@0.1.2";
        "@types/source-list-map@0.1.2" = f (sc "types" "source-list-map") "0.1.2" (ir "https://registry.yarnpkg.com/@types/source-list-map/-/source-list-map-0.1.2.tgz") "0078836063ffaf17412349bba364087e0ac02ec9" [];
        "@types/tapable@1.0.8" = f (sc "types" "tapable") "1.0.8" (ir "https://registry.yarnpkg.com/@types/tapable/-/tapable-1.0.8.tgz") "b94a4391c85666c7b73299fd3ad79d4faa435310" [];
        "@types/tapable@^1" = s."@types/tapable@1.0.8";
        "@types/tapable@^1.0.5" = s."@types/tapable@1.0.8";
        "@types/uglify-js@*" = s."@types/uglify-js@3.13.1";
        "@types/uglify-js@3.13.1" = f (sc "types" "uglify-js") "3.13.1" (ir "https://registry.yarnpkg.com/@types/uglify-js/-/uglify-js-3.13.1.tgz") "5e889e9e81e94245c75b6450600e1c5ea2878aea" [
          (s."source-map@^0.6.1")
          ];
        "@types/unist@*" = s."@types/unist@2.0.6";
        "@types/unist@2.0.6" = f (sc "types" "unist") "2.0.6" (ir "https://registry.yarnpkg.com/@types/unist/-/unist-2.0.6.tgz") "250a7b16c3b91f672a24552ec64678eeb1d3a08d" [];
        "@types/unist@^2.0.0" = s."@types/unist@2.0.6";
        "@types/unist@^2.0.2" = s."@types/unist@2.0.6";
        "@types/unist@^2.0.3" = s."@types/unist@2.0.6";
        "@types/webpack-env@1.16.3" = f (sc "types" "webpack-env") "1.16.3" (ir "https://registry.yarnpkg.com/@types/webpack-env/-/webpack-env-1.16.3.tgz") "b776327a73e561b71e7881d0cd6d34a1424db86a" [];
        "@types/webpack-env@^1.16.0" = s."@types/webpack-env@1.16.3";
        "@types/webpack-sources@*" = s."@types/webpack-sources@3.2.0";
        "@types/webpack-sources@3.2.0" = f (sc "types" "webpack-sources") "3.2.0" (ir "https://registry.yarnpkg.com/@types/webpack-sources/-/webpack-sources-3.2.0.tgz") "16d759ba096c289034b26553d2df1bf45248d38b" [
          (s."@types/node@*")
          (s."@types/source-list-map@*")
          (s."source-map@^0.7.3")
          ];
        "@types/webpack@4.41.32" = f (sc "types" "webpack") "4.41.32" (ir "https://registry.yarnpkg.com/@types/webpack/-/webpack-4.41.32.tgz") "a7bab03b72904070162b2f169415492209e94212" [
          (s."@types/node@*")
          (s."@types/tapable@^1")
          (s."@types/uglify-js@*")
          (s."@types/webpack-sources@*")
          (s."anymatch@^3.0.0")
          (s."source-map@^0.6.0")
          ];
        "@types/webpack@^4.41.26" = s."@types/webpack@4.41.32";
        "@types/webpack@^4.41.8" = s."@types/webpack@4.41.32";
        "@types/yargs-parser@*" = s."@types/yargs-parser@20.2.1";
        "@types/yargs-parser@20.2.1" = f (sc "types" "yargs-parser") "20.2.1" (ir "https://registry.yarnpkg.com/@types/yargs-parser/-/yargs-parser-20.2.1.tgz") "3b9ce2489919d9e4fea439b76916abc34b2df129" [];
        "@types/yargs@15.0.14" = f (sc "types" "yargs") "15.0.14" (ir "https://registry.yarnpkg.com/@types/yargs/-/yargs-15.0.14.tgz") "26d821ddb89e70492160b66d10a0eb6df8f6fb06" [
          (s."@types/yargs-parser@*")
          ];
        "@types/yargs@^15.0.0" = s."@types/yargs@15.0.14";
        "@webassemblyjs/ast@1.9.0" = f (sc "webassemblyjs" "ast") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/ast/-/ast-1.9.0.tgz") "bd850604b4042459a5a41cd7d338cbed695ed964" [
          (s."@webassemblyjs/helper-module-context@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/wast-parser@1.9.0")
          ];
        "@webassemblyjs/floating-point-hex-parser@1.9.0" = f (sc "webassemblyjs" "floating-point-hex-parser") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/floating-point-hex-parser/-/floating-point-hex-parser-1.9.0.tgz") "3c3d3b271bddfc84deb00f71344438311d52ffb4" [];
        "@webassemblyjs/helper-api-error@1.9.0" = f (sc "webassemblyjs" "helper-api-error") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-api-error/-/helper-api-error-1.9.0.tgz") "203f676e333b96c9da2eeab3ccef33c45928b6a2" [];
        "@webassemblyjs/helper-buffer@1.9.0" = f (sc "webassemblyjs" "helper-buffer") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-buffer/-/helper-buffer-1.9.0.tgz") "a1442d269c5feb23fcbc9ef759dac3547f29de00" [];
        "@webassemblyjs/helper-code-frame@1.9.0" = f (sc "webassemblyjs" "helper-code-frame") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-code-frame/-/helper-code-frame-1.9.0.tgz") "647f8892cd2043a82ac0c8c5e75c36f1d9159f27" [
          (s."@webassemblyjs/wast-printer@1.9.0")
          ];
        "@webassemblyjs/helper-fsm@1.9.0" = f (sc "webassemblyjs" "helper-fsm") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-fsm/-/helper-fsm-1.9.0.tgz") "c05256b71244214671f4b08ec108ad63b70eddb8" [];
        "@webassemblyjs/helper-module-context@1.9.0" = f (sc "webassemblyjs" "helper-module-context") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-module-context/-/helper-module-context-1.9.0.tgz") "25d8884b76839871a08a6c6f806c3979ef712f07" [];
        "@webassemblyjs/helper-wasm-bytecode@1.9.0" = f (sc "webassemblyjs" "helper-wasm-bytecode") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-wasm-bytecode/-/helper-wasm-bytecode-1.9.0.tgz") "4fed8beac9b8c14f8c58b70d124d549dd1fe5790" [];
        "@webassemblyjs/helper-wasm-section@1.9.0" = f (sc "webassemblyjs" "helper-wasm-section") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/helper-wasm-section/-/helper-wasm-section-1.9.0.tgz") "5a4138d5a6292ba18b04c5ae49717e4167965346" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-buffer@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/wasm-gen@1.9.0")
          ];
        "@webassemblyjs/ieee754@1.9.0" = f (sc "webassemblyjs" "ieee754") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/ieee754/-/ieee754-1.9.0.tgz") "15c7a0fbaae83fb26143bbacf6d6df1702ad39e4" [
          (s."@xtuc/ieee754@^1.2.0")
          ];
        "@webassemblyjs/leb128@1.9.0" = f (sc "webassemblyjs" "leb128") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/leb128/-/leb128-1.9.0.tgz") "f19ca0b76a6dc55623a09cffa769e838fa1e1c95" [
          (s."@xtuc/long@4.2.2")
          ];
        "@webassemblyjs/utf8@1.9.0" = f (sc "webassemblyjs" "utf8") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/utf8/-/utf8-1.9.0.tgz") "04d33b636f78e6a6813227e82402f7637b6229ab" [];
        "@webassemblyjs/wasm-edit@1.9.0" = f (sc "webassemblyjs" "wasm-edit") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wasm-edit/-/wasm-edit-1.9.0.tgz") "3fe6d79d3f0f922183aa86002c42dd256cfee9cf" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-buffer@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/helper-wasm-section@1.9.0")
          (s."@webassemblyjs/wasm-gen@1.9.0")
          (s."@webassemblyjs/wasm-opt@1.9.0")
          (s."@webassemblyjs/wasm-parser@1.9.0")
          (s."@webassemblyjs/wast-printer@1.9.0")
          ];
        "@webassemblyjs/wasm-gen@1.9.0" = f (sc "webassemblyjs" "wasm-gen") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wasm-gen/-/wasm-gen-1.9.0.tgz") "50bc70ec68ded8e2763b01a1418bf43491a7a49c" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/ieee754@1.9.0")
          (s."@webassemblyjs/leb128@1.9.0")
          (s."@webassemblyjs/utf8@1.9.0")
          ];
        "@webassemblyjs/wasm-opt@1.9.0" = f (sc "webassemblyjs" "wasm-opt") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wasm-opt/-/wasm-opt-1.9.0.tgz") "2211181e5b31326443cc8112eb9f0b9028721a61" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-buffer@1.9.0")
          (s."@webassemblyjs/wasm-gen@1.9.0")
          (s."@webassemblyjs/wasm-parser@1.9.0")
          ];
        "@webassemblyjs/wasm-parser@1.9.0" = f (sc "webassemblyjs" "wasm-parser") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wasm-parser/-/wasm-parser-1.9.0.tgz") "9d48e44826df4a6598294aa6c87469d642fff65e" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-api-error@1.9.0")
          (s."@webassemblyjs/helper-wasm-bytecode@1.9.0")
          (s."@webassemblyjs/ieee754@1.9.0")
          (s."@webassemblyjs/leb128@1.9.0")
          (s."@webassemblyjs/utf8@1.9.0")
          ];
        "@webassemblyjs/wast-parser@1.9.0" = f (sc "webassemblyjs" "wast-parser") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wast-parser/-/wast-parser-1.9.0.tgz") "3031115d79ac5bd261556cecc3fa90a3ef451914" [
          (s."@webassemblyjs/floating-point-hex-parser@1.9.0")
          (s."@webassemblyjs/helper-api-error@1.9.0")
          (s."@webassemblyjs/helper-code-frame@1.9.0")
          (s."@webassemblyjs/helper-fsm@1.9.0")
          (s."@xtuc/long@4.2.2")
          ];
        "@webassemblyjs/wast-printer@1.9.0" = f (sc "webassemblyjs" "wast-printer") "1.9.0" (ir "https://registry.yarnpkg.com/@webassemblyjs/wast-printer/-/wast-printer-1.9.0.tgz") "4935d54c85fef637b00ce9f52377451d00d47899" [
          (s."@xtuc/long@4.2.2")
          ];
        "@xtuc/ieee754@1.2.0" = f (sc "xtuc" "ieee754") "1.2.0" (ir "https://registry.yarnpkg.com/@xtuc/ieee754/-/ieee754-1.2.0.tgz") "eef014a3145ae477a1cbc00cd1e552336dceb790" [];
        "@xtuc/ieee754@^1.2.0" = s."@xtuc/ieee754@1.2.0";
        "@xtuc/long@4.2.2" = f (sc "xtuc" "long") "4.2.2" (ir "https://registry.yarnpkg.com/@xtuc/long/-/long-4.2.2.tgz") "d291c6a4e97989b5c61d9acf396ae4fe133a718d" [];
        "accepts@1.3.7" = f "accepts" "1.3.7" y "531bc726517a3b2b41f850021c6cc15eaab507cd" [
          (s."mime-types@~2.1.24")
          (s."negotiator@0.6.2")
          ];
        "accepts@~1.3.5" = s."accepts@1.3.7";
        "accepts@~1.3.7" = s."accepts@1.3.7";
        "acorn-jsx@5.3.2" = f "acorn-jsx" "5.3.2" y "7ed5bb55908b3b2f1bc55c6af1653bada7f07937" [];
        "acorn-jsx@^5.3.1" = s."acorn-jsx@5.3.2";
        "acorn-walk@7.2.0" = f "acorn-walk" "7.2.0" y "0de889a601203909b0fbe07b8938dc21d2e967bc" [];
        "acorn-walk@^7.2.0" = s."acorn-walk@7.2.0";
        "acorn@6.4.2" = f "acorn" "6.4.2" y "35866fd710528e92de10cf06016498e47e39e1e6" [];
        "acorn@7.4.1" = f "acorn" "7.4.1" y "feaed255973d2e77555b83dbc08851a6c63520fa" [];
        "acorn@^6.4.1" = s."acorn@6.4.2";
        "acorn@^7.4.1" = s."acorn@7.4.1";
        "address@1.1.2" = f "address" "1.1.2" y "bf1116c9c758c51b7a933d296b72c221ed9428b6" [];
        "address@^1.0.1" = s."address@1.1.2";
        "aggregate-error@3.1.0" = f "aggregate-error" "3.1.0" y "92670ff50f5359bdb7a3e0d40d0ec30c5737687a" [
          (s."clean-stack@^2.0.0")
          (s."indent-string@^4.0.0")
          ];
        "aggregate-error@^3.0.0" = s."aggregate-error@3.1.0";
        "airbnb-js-shims@2.2.1" = f "airbnb-js-shims" "2.2.1" y "db481102d682b98ed1daa4c5baa697a05ce5c040" [
          (s."array-includes@^3.0.3")
          (s."array.prototype.flat@^1.2.1")
          (s."array.prototype.flatmap@^1.2.1")
          (s."es5-shim@^4.5.13")
          (s."es6-shim@^0.35.5")
          (s."function.prototype.name@^1.1.0")
          (s."globalthis@^1.0.0")
          (s."object.entries@^1.1.0")
          (s."object.fromentries@^2.0.0 || ^1.0.0")
          (s."object.getownpropertydescriptors@^2.0.3")
          (s."object.values@^1.1.0")
          (s."promise.allsettled@^1.0.0")
          (s."promise.prototype.finally@^3.1.0")
          (s."string.prototype.matchall@^4.0.0 || ^3.0.1")
          (s."string.prototype.padend@^3.0.0")
          (s."string.prototype.padstart@^3.0.0")
          (s."symbol.prototype.description@^1.0.0")
          ];
        "airbnb-js-shims@^2.2.1" = s."airbnb-js-shims@2.2.1";
        "ajv-errors@1.0.1" = f "ajv-errors" "1.0.1" y "f35986aceb91afadec4102fbd85014950cefa64d" [];
        "ajv-errors@^1.0.0" = s."ajv-errors@1.0.1";
        "ajv-keywords@3.5.2" = f "ajv-keywords" "3.5.2" y "31f29da5ab6e00d1c2d329acf7b5929614d5014d" [];
        "ajv-keywords@^3.1.0" = s."ajv-keywords@3.5.2";
        "ajv-keywords@^3.4.1" = s."ajv-keywords@3.5.2";
        "ajv-keywords@^3.5.2" = s."ajv-keywords@3.5.2";
        "ajv@6.12.6" = f "ajv" "6.12.6" y "baf5a62e802b07d977034586f8c3baf5adf26df4" [
          (s."fast-deep-equal@^3.1.1")
          (s."fast-json-stable-stringify@^2.0.0")
          (s."json-schema-traverse@^0.4.1")
          (s."uri-js@^4.2.2")
          ];
        "ajv@^6.1.0" = s."ajv@6.12.6";
        "ajv@^6.10.2" = s."ajv@6.12.6";
        "ajv@^6.12.2" = s."ajv@6.12.6";
        "ajv@^6.12.4" = s."ajv@6.12.6";
        "ajv@^6.12.5" = s."ajv@6.12.6";
        "ansi-align@3.0.1" = f "ansi-align" "3.0.1" y "0cdf12e111ace773a86e9a1fad1225c43cb19a59" [
          (s."string-width@^4.1.0")
          ];
        "ansi-align@^3.0.0" = s."ansi-align@3.0.1";
        "ansi-colors@3.2.4" = f "ansi-colors" "3.2.4" y "e3a3da4bfbae6c86a9c285625de124a234026fbf" [];
        "ansi-colors@^3.0.0" = s."ansi-colors@3.2.4";
        "ansi-html-community@0.0.8" = f "ansi-html-community" "0.0.8" y "69fbc4d6ccbe383f9736934ae34c3f8290f1bf41" [];
        "ansi-html-community@^0.0.8" = s."ansi-html-community@0.0.8";
        "ansi-regex@2.1.1" = f "ansi-regex" "2.1.1" y "c3b33ab5ee360d86e0e628f0468ae7ef27d654df" [];
        "ansi-regex@5.0.1" = f "ansi-regex" "5.0.1" y "082cb2c89c9fe8659a311a53bd6a4dc5301db304" [];
        "ansi-regex@^2.0.0" = s."ansi-regex@2.1.1";
        "ansi-regex@^5.0.1" = s."ansi-regex@5.0.1";
        "ansi-styles@3.2.1" = f "ansi-styles" "3.2.1" y "41fbb20243e50b12be0f04b8dedbf07520ce841d" [
          (s."color-convert@^1.9.0")
          ];
        "ansi-styles@4.3.0" = f "ansi-styles" "4.3.0" y "edd803628ae71c04c85ae7a0906edad34b648937" [
          (s."color-convert@^2.0.1")
          ];
        "ansi-styles@^3.2.1" = s."ansi-styles@3.2.1";
        "ansi-styles@^4.0.0" = s."ansi-styles@4.3.0";
        "ansi-styles@^4.1.0" = s."ansi-styles@4.3.0";
        "ansi-to-html@0.6.15" = f "ansi-to-html" "0.6.15" y "ac6ad4798a00f6aa045535d7f6a9cb9294eebea7" [
          (s."entities@^2.0.0")
          ];
        "ansi-to-html@^0.6.11" = s."ansi-to-html@0.6.15";
        "anymatch@2.0.0" = f "anymatch" "2.0.0" y "bcb24b4f37934d9aa7ac17b4adaf89e7c76ef2eb" [
          (s."micromatch@^3.1.4")
          (s."normalize-path@^2.1.1")
          ];
        "anymatch@3.1.2" = f "anymatch" "3.1.2" y "c0557c096af32f106198f4f4e2a383537e378716" [
          (s."normalize-path@^3.0.0")
          (s."picomatch@^2.0.4")
          ];
        "anymatch@^2.0.0" = s."anymatch@2.0.0";
        "anymatch@^3.0.0" = s."anymatch@3.1.2";
        "anymatch@^3.0.3" = s."anymatch@3.1.2";
        "anymatch@~3.1.2" = s."anymatch@3.1.2";
        "app-root-dir@1.0.2" = f "app-root-dir" "1.0.2" y "38187ec2dea7577fff033ffcb12172692ff6e118" [];
        "app-root-dir@^1.0.2" = s."app-root-dir@1.0.2";
        "aproba@1.2.0" = f "aproba" "1.2.0" y "6802e6264efd18c790a1b0d517f0f2627bf2c94a" [];
        "aproba@2.0.0" = f "aproba" "2.0.0" y "52520b8ae5b569215b354efc0caa3fe1e45a8adc" [];
        "aproba@^1.0.3 || ^2.0.0" = s."aproba@2.0.0";
        "aproba@^1.1.1" = s."aproba@1.2.0";
        "are-we-there-yet@2.0.0" = f "are-we-there-yet" "2.0.0" y "372e0e7bd279d8e94c653aaa1f67200884bf3e1c" [
          (s."delegates@^1.0.0")
          (s."readable-stream@^3.6.0")
          ];
        "are-we-there-yet@^2.0.0" = s."are-we-there-yet@2.0.0";
        "argparse@1.0.10" = f "argparse" "1.0.10" y "bcd6791ea5ae09725e17e5ad988134cd40b3d911" [
          (s."sprintf-js@~1.0.2")
          ];
        "argparse@^1.0.7" = s."argparse@1.0.10";
        "arr-diff@4.0.0" = f "arr-diff" "4.0.0" y "d6461074febfec71e7e15235761a329a5dc7c520" [];
        "arr-diff@^4.0.0" = s."arr-diff@4.0.0";
        "arr-flatten@1.1.0" = f "arr-flatten" "1.1.0" y "36048bbff4e7b47e136644316c99669ea5ae91f1" [];
        "arr-flatten@^1.1.0" = s."arr-flatten@1.1.0";
        "arr-union@3.1.0" = f "arr-union" "3.1.0" y "e39b09aea9def866a8f206e288af63919bae39c4" [];
        "arr-union@^3.1.0" = s."arr-union@3.1.0";
        "array-flatten@1.1.1" = f "array-flatten" "1.1.1" y "9a5f699051b1e7073328f2a008968b64ea2955d2" [];
        "array-includes@3.1.4" = f "array-includes" "3.1.4" y "f5b493162c760f3539631f005ba2bb46acb45ba9" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          (s."get-intrinsic@^1.1.1")
          (s."is-string@^1.0.7")
          ];
        "array-includes@^3.0.3" = s."array-includes@3.1.4";
        "array-union@1.0.2" = f "array-union" "1.0.2" y "9a34410e4f4e3da23dea375be5be70f24778ec39" [
          (s."array-uniq@^1.0.1")
          ];
        "array-union@2.1.0" = f "array-union" "2.1.0" y "b798420adbeb1de828d84acd8a2e23d3efe85e8d" [];
        "array-union@^1.0.2" = s."array-union@1.0.2";
        "array-union@^2.1.0" = s."array-union@2.1.0";
        "array-uniq@1.0.3" = f "array-uniq" "1.0.3" y "af6ac877a25cc7f74e058894753858dfdb24fdb6" [];
        "array-uniq@^1.0.1" = s."array-uniq@1.0.3";
        "array-unique@0.3.2" = f "array-unique" "0.3.2" y "a894b75d4bc4f6cd679ef3244a9fd8f46ae2d428" [];
        "array-unique@^0.3.2" = s."array-unique@0.3.2";
        "array.prototype.flat@1.2.5" = f "array.prototype.flat" "1.2.5" y "07e0975d84bbc7c48cd1879d609e682598d33e13" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.0")
          ];
        "array.prototype.flat@^1.2.1" = s."array.prototype.flat@1.2.5";
        "array.prototype.flatmap@1.2.5" = f "array.prototype.flatmap" "1.2.5" y "908dc82d8a406930fdf38598d51e7411d18d4446" [
          (s."call-bind@^1.0.0")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.0")
          ];
        "array.prototype.flatmap@^1.2.1" = s."array.prototype.flatmap@1.2.5";
        "array.prototype.map@1.0.4" = f "array.prototype.map" "1.0.4" y "0d97b640cfdd036c1b41cfe706a5e699aa0711f2" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.0")
          (s."es-array-method-boxes-properly@^1.0.0")
          (s."is-string@^1.0.7")
          ];
        "array.prototype.map@^1.0.4" = s."array.prototype.map@1.0.4";
        "arrify@2.0.1" = f "arrify" "2.0.1" y "c9655e9331e0abcd588d2a7cad7e9956f66701fa" [];
        "arrify@^2.0.1" = s."arrify@2.0.1";
        "asn1.js@5.4.1" = f "asn1.js" "5.4.1" y "11a980b84ebb91781ce35b0fdc2ee294e3783f07" [
          (s."bn.js@^4.0.0")
          (s."inherits@^2.0.1")
          (s."minimalistic-assert@^1.0.0")
          (s."safer-buffer@^2.1.0")
          ];
        "asn1.js@^5.2.0" = s."asn1.js@5.4.1";
        "assert@1.5.0" = f "assert" "1.5.0" y "55c109aaf6e0aefdb3dc4b71240c70bf574b18eb" [
          (s."object-assign@^4.1.1")
          (s."util@0.10.3")
          ];
        "assert@^1.1.1" = s."assert@1.5.0";
        "assign-symbols@1.0.0" = f "assign-symbols" "1.0.0" y "59667f41fadd4f20ccbc2bb96b8d4f7f78ec0367" [];
        "assign-symbols@^1.0.0" = s."assign-symbols@1.0.0";
        "ast-types@0.14.2" = f "ast-types" "0.14.2" y "600b882df8583e3cd4f2df5fa20fa83759d4bdfd" [
          (s."tslib@^2.0.1")
          ];
        "ast-types@^0.14.2" = s."ast-types@0.14.2";
        "async-each@1.0.3" = f "async-each" "1.0.3" y "b727dbf87d7651602f06f4d4ac387f47d91b0cbf" [];
        "async-each@^1.0.1" = s."async-each@1.0.3";
        "asynckit@0.4.0" = f "asynckit" "0.4.0" y "c79ed97f7f34cb8f2ba1bc9790bcc366474b4b79" [];
        "asynckit@^0.4.0" = s."asynckit@0.4.0";
        "at-least-node@1.0.0" = f "at-least-node" "1.0.0" y "602cd4b46e844ad4effc92a8011a3c46e0238dc2" [];
        "at-least-node@^1.0.0" = s."at-least-node@1.0.0";
        "atob@2.1.2" = f "atob" "2.1.2" y "6d9517eb9e030d2436666651e86bd9f6f13533c9" [];
        "atob@^2.1.2" = s."atob@2.1.2";
        "autoprefixer@9.8.8" = f "autoprefixer" "9.8.8" y "fd4bd4595385fa6f06599de749a4d5f7a474957a" [
          (s."browserslist@^4.12.0")
          (s."caniuse-lite@^1.0.30001109")
          (s."normalize-range@^0.1.2")
          (s."num2fraction@^1.2.2")
          (s."picocolors@^0.2.1")
          (s."postcss@^7.0.32")
          (s."postcss-value-parser@^4.1.0")
          ];
        "autoprefixer@^9.8.6" = s."autoprefixer@9.8.8";
        "babel-loader@8.2.3" = f "babel-loader" "8.2.3" y "8986b40f1a64cacfcb4b8429320085ef68b1342d" [
          (s."find-cache-dir@^3.3.1")
          (s."loader-utils@^1.4.0")
          (s."make-dir@^3.1.0")
          (s."schema-utils@^2.6.5")
          ];
        "babel-loader@^8.0.0" = s."babel-loader@8.2.3";
        "babel-loader@^8.2.3" = s."babel-loader@8.2.3";
        "babel-plugin-add-react-displayname@0.0.5" = f "babel-plugin-add-react-displayname" "0.0.5" y "339d4cddb7b65fd62d1df9db9fe04de134122bd5" [];
        "babel-plugin-add-react-displayname@^0.0.5" = s."babel-plugin-add-react-displayname@0.0.5";
        "babel-plugin-apply-mdx-type-prop@1.6.22" = f "babel-plugin-apply-mdx-type-prop" "1.6.22" y "d216e8fd0de91de3f1478ef3231e05446bc8705b" [
          (s."@babel/helper-plugin-utils@7.10.4")
          (s."@mdx-js/util@1.6.22")
          ];
        "babel-plugin-dynamic-import-node@2.3.3" = f "babel-plugin-dynamic-import-node" "2.3.3" y "84fda19c976ec5c6defef57f9427b3def66e17a3" [
          (s."object.assign@^4.1.0")
          ];
        "babel-plugin-dynamic-import-node@^2.3.3" = s."babel-plugin-dynamic-import-node@2.3.3";
        "babel-plugin-emotion@10.2.2" = f "babel-plugin-emotion" "10.2.2" y "a1fe3503cff80abfd0bdda14abd2e8e57a79d17d" [
          (s."@babel/helper-module-imports@^7.0.0")
          (s."@emotion/hash@0.8.0")
          (s."@emotion/memoize@0.7.4")
          (s."@emotion/serialize@^0.11.16")
          (s."babel-plugin-macros@^2.0.0")
          (s."babel-plugin-syntax-jsx@^6.18.0")
          (s."convert-source-map@^1.5.0")
          (s."escape-string-regexp@^1.0.5")
          (s."find-root@^1.1.0")
          (s."source-map@^0.5.7")
          ];
        "babel-plugin-emotion@^10.0.27" = s."babel-plugin-emotion@10.2.2";
        "babel-plugin-extract-import-names@1.6.22" = f "babel-plugin-extract-import-names" "1.6.22" y "de5f9a28eb12f3eb2578bf74472204e66d1a13dc" [
          (s."@babel/helper-plugin-utils@7.10.4")
          ];
        "babel-plugin-istanbul@6.1.1" = f "babel-plugin-istanbul" "6.1.1" y "fa88ec59232fd9b4e36dbbc540a8ec9a9b47da73" [
          (s."@babel/helper-plugin-utils@^7.0.0")
          (s."@istanbuljs/load-nyc-config@^1.0.0")
          (s."@istanbuljs/schema@^0.1.2")
          (s."istanbul-lib-instrument@^5.0.4")
          (s."test-exclude@^6.0.0")
          ];
        "babel-plugin-istanbul@^6.0.0" = s."babel-plugin-istanbul@6.1.1";
        "babel-plugin-macros@2.8.0" = f "babel-plugin-macros" "2.8.0" y "0f958a7cc6556b1e65344465d99111a1e5e10138" [
          (s."@babel/runtime@^7.7.2")
          (s."cosmiconfig@^6.0.0")
          (s."resolve@^1.12.0")
          ];
        "babel-plugin-macros@3.1.0" = f "babel-plugin-macros" "3.1.0" y "9ef6dc74deb934b4db344dc973ee851d148c50c1" [
          (s."@babel/runtime@^7.12.5")
          (s."cosmiconfig@^7.0.0")
          (s."resolve@^1.19.0")
          ];
        "babel-plugin-macros@^2.0.0" = s."babel-plugin-macros@2.8.0";
        "babel-plugin-macros@^2.8.0" = s."babel-plugin-macros@2.8.0";
        "babel-plugin-macros@^3.0.1" = s."babel-plugin-macros@3.1.0";
        "babel-plugin-named-asset-import@0.3.8" = f "babel-plugin-named-asset-import" "0.3.8" y "6b7fa43c59229685368683c28bc9734f24524cc2" [];
        "babel-plugin-named-asset-import@^0.3.1" = s."babel-plugin-named-asset-import@0.3.8";
        "babel-plugin-polyfill-corejs2@0.3.1" = f "babel-plugin-polyfill-corejs2" "0.3.1" y "440f1b70ccfaabc6b676d196239b138f8a2cfba5" [
          (s."@babel/compat-data@^7.13.11")
          (s."@babel/helper-define-polyfill-provider@^0.3.1")
          (s."semver@^6.1.1")
          ];
        "babel-plugin-polyfill-corejs2@^0.3.0" = s."babel-plugin-polyfill-corejs2@0.3.1";
        "babel-plugin-polyfill-corejs3@0.1.7" = f "babel-plugin-polyfill-corejs3" "0.1.7" y "80449d9d6f2274912e05d9e182b54816904befd0" [
          (s."@babel/helper-define-polyfill-provider@^0.1.5")
          (s."core-js-compat@^3.8.1")
          ];
        "babel-plugin-polyfill-corejs3@0.5.1" = f "babel-plugin-polyfill-corejs3" "0.5.1" y "d66183bf10976ea677f4149a7fcc4d8df43d4060" [
          (s."@babel/helper-define-polyfill-provider@^0.3.1")
          (s."core-js-compat@^3.20.0")
          ];
        "babel-plugin-polyfill-corejs3@^0.1.0" = s."babel-plugin-polyfill-corejs3@0.1.7";
        "babel-plugin-polyfill-corejs3@^0.5.0" = s."babel-plugin-polyfill-corejs3@0.5.1";
        "babel-plugin-polyfill-regenerator@0.3.1" = f "babel-plugin-polyfill-regenerator" "0.3.1" y "2c0678ea47c75c8cc2fbb1852278d8fb68233990" [
          (s."@babel/helper-define-polyfill-provider@^0.3.1")
          ];
        "babel-plugin-polyfill-regenerator@^0.3.0" = s."babel-plugin-polyfill-regenerator@0.3.1";
        "babel-plugin-react-docgen@4.2.1" = f "babel-plugin-react-docgen" "4.2.1" y "7cc8e2f94e8dc057a06e953162f0810e4e72257b" [
          (s."ast-types@^0.14.2")
          (s."lodash@^4.17.15")
          (s."react-docgen@^5.0.0")
          ];
        "babel-plugin-react-docgen@^4.2.1" = s."babel-plugin-react-docgen@4.2.1";
        "babel-plugin-syntax-jsx@6.18.0" = f "babel-plugin-syntax-jsx" "6.18.0" y "0af32a9a6e13ca7a3fd5069e62d7b0f58d0d8946" [];
        "babel-plugin-syntax-jsx@^6.18.0" = s."babel-plugin-syntax-jsx@6.18.0";
        "bail@1.0.5" = f "bail" "1.0.5" y "b6fa133404a392cbc1f8c4bf63f5953351e7a776" [];
        "bail@^1.0.0" = s."bail@1.0.5";
        "balanced-match@1.0.2" = f "balanced-match" "1.0.2" y "e83e3a7e3f300b34cb9d87f615fa0cbf357690ee" [];
        "balanced-match@^1.0.0" = s."balanced-match@1.0.2";
        "base64-js@1.5.1" = f "base64-js" "1.5.1" y "1b1b440160a5bf7ad40b650f095963481903930a" [];
        "base64-js@^1.0.2" = s."base64-js@1.5.1";
        "base@0.11.2" = f "base" "0.11.2" y "7bde5ced145b6d551a90db87f83c558b4eb48a8f" [
          (s."cache-base@^1.0.1")
          (s."class-utils@^0.3.5")
          (s."component-emitter@^1.2.1")
          (s."define-property@^1.0.0")
          (s."isobject@^3.0.1")
          (s."mixin-deep@^1.2.0")
          (s."pascalcase@^0.1.1")
          ];
        "base@^0.11.1" = s."base@0.11.2";
        "batch-processor@1.0.0" = f "batch-processor" "1.0.0" y "75c95c32b748e0850d10c2b168f6bdbe9891ace8" [];
        "better-opn@2.1.1" = f "better-opn" "2.1.1" y "94a55b4695dc79288f31d7d0e5f658320759f7c6" [
          (s."open@^7.0.3")
          ];
        "better-opn@^2.1.1" = s."better-opn@2.1.1";
        "big.js@5.2.2" = f "big.js" "5.2.2" y "65f0af382f578bcdc742bd9c281e9cb2d7768328" [];
        "big.js@^5.2.2" = s."big.js@5.2.2";
        "binary-extensions@1.13.1" = f "binary-extensions" "1.13.1" y "598afe54755b2868a5330d2aff9d4ebb53209b65" [];
        "binary-extensions@2.2.0" = f "binary-extensions" "2.2.0" y "75f502eeaf9ffde42fc98829645be4ea76bd9e2d" [];
        "binary-extensions@^1.0.0" = s."binary-extensions@1.13.1";
        "binary-extensions@^2.0.0" = s."binary-extensions@2.2.0";
        "bindings@1.5.0" = f "bindings" "1.5.0" y "10353c9e945334bc0511a6d90b38fbc7c9c504df" [
          (s."file-uri-to-path@1.0.0")
          ];
        "bindings@^1.5.0" = s."bindings@1.5.0";
        "bluebird@3.7.2" = f "bluebird" "3.7.2" y "9f229c15be272454ffa973ace0dbee79a1b0c36f" [];
        "bluebird@^3.3.5" = s."bluebird@3.7.2";
        "bluebird@^3.5.5" = s."bluebird@3.7.2";
        "bn.js@4.12.0" = f "bn.js" "4.12.0" y "775b3f278efbb9718eec7361f483fb36fbbfea88" [];
        "bn.js@5.2.0" = f "bn.js" "5.2.0" y "358860674396c6997771a9d051fcc1b57d4ae002" [];
        "bn.js@^4.0.0" = s."bn.js@4.12.0";
        "bn.js@^4.1.0" = s."bn.js@4.12.0";
        "bn.js@^4.11.9" = s."bn.js@4.12.0";
        "bn.js@^5.0.0" = s."bn.js@5.2.0";
        "bn.js@^5.1.1" = s."bn.js@5.2.0";
        "body-parser@1.19.1" = f "body-parser" "1.19.1" y "1499abbaa9274af3ecc9f6f10396c995943e31d4" [
          (s."bytes@3.1.1")
          (s."content-type@~1.0.4")
          (s."debug@2.6.9")
          (s."depd@~1.1.2")
          (s."http-errors@1.8.1")
          (s."iconv-lite@0.4.24")
          (s."on-finished@~2.3.0")
          (s."qs@6.9.6")
          (s."raw-body@2.4.2")
          (s."type-is@~1.6.18")
          ];
        "boolbase@1.0.0" = f "boolbase" "1.0.0" y "68dff5fbe60c51eb37725ea9e3ed310dcc1e776e" [];
        "boolbase@^1.0.0" = s."boolbase@1.0.0";
        "boxen@5.1.2" = f "boxen" "5.1.2" y "788cb686fc83c1f486dfa8a40c68fc2b831d2b50" [
          (s."ansi-align@^3.0.0")
          (s."camelcase@^6.2.0")
          (s."chalk@^4.1.0")
          (s."cli-boxes@^2.2.1")
          (s."string-width@^4.2.2")
          (s."type-fest@^0.20.2")
          (s."widest-line@^3.1.0")
          (s."wrap-ansi@^7.0.0")
          ];
        "boxen@^5.1.2" = s."boxen@5.1.2";
        "brace-expansion@1.1.11" = f "brace-expansion" "1.1.11" y "3c7fcbf529d87226f3d2f52b966ff5271eb441dd" [
          (s."balanced-match@^1.0.0")
          (s."concat-map@0.0.1")
          ];
        "brace-expansion@^1.1.7" = s."brace-expansion@1.1.11";
        "braces@2.3.2" = f "braces" "2.3.2" y "5979fd3f14cd531565e5fa2df1abfff1dfaee729" [
          (s."arr-flatten@^1.1.0")
          (s."array-unique@^0.3.2")
          (s."extend-shallow@^2.0.1")
          (s."fill-range@^4.0.0")
          (s."isobject@^3.0.1")
          (s."repeat-element@^1.1.2")
          (s."snapdragon@^0.8.1")
          (s."snapdragon-node@^2.0.1")
          (s."split-string@^3.0.2")
          (s."to-regex@^3.0.1")
          ];
        "braces@3.0.2" = f "braces" "3.0.2" y "3454e1a462ee8d599e236df336cd9ea4f8afe107" [
          (s."fill-range@^7.0.1")
          ];
        "braces@^2.3.1" = s."braces@2.3.2";
        "braces@^2.3.2" = s."braces@2.3.2";
        "braces@^3.0.1" = s."braces@3.0.2";
        "braces@~3.0.2" = s."braces@3.0.2";
        "brorand@1.1.0" = f "brorand" "1.1.0" y "12c25efe40a45e3c323eb8675a0a0ce57b22371f" [];
        "brorand@^1.0.1" = s."brorand@1.1.0";
        "brorand@^1.1.0" = s."brorand@1.1.0";
        "browserify-aes@1.2.0" = f "browserify-aes" "1.2.0" y "326734642f403dabc3003209853bb70ad428ef48" [
          (s."buffer-xor@^1.0.3")
          (s."cipher-base@^1.0.0")
          (s."create-hash@^1.1.0")
          (s."evp_bytestokey@^1.0.3")
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.0.1")
          ];
        "browserify-aes@^1.0.0" = s."browserify-aes@1.2.0";
        "browserify-aes@^1.0.4" = s."browserify-aes@1.2.0";
        "browserify-cipher@1.0.1" = f "browserify-cipher" "1.0.1" y "8d6474c1b870bfdabcd3bcfcc1934a10e94f15f0" [
          (s."browserify-aes@^1.0.4")
          (s."browserify-des@^1.0.0")
          (s."evp_bytestokey@^1.0.0")
          ];
        "browserify-cipher@^1.0.0" = s."browserify-cipher@1.0.1";
        "browserify-des@1.0.2" = f "browserify-des" "1.0.2" y "3af4f1f59839403572f1c66204375f7a7f703e9c" [
          (s."cipher-base@^1.0.1")
          (s."des.js@^1.0.0")
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.1.2")
          ];
        "browserify-des@^1.0.0" = s."browserify-des@1.0.2";
        "browserify-rsa@4.1.0" = f "browserify-rsa" "4.1.0" y "b2fd06b5b75ae297f7ce2dc651f918f5be158c8d" [
          (s."bn.js@^5.0.0")
          (s."randombytes@^2.0.1")
          ];
        "browserify-rsa@^4.0.0" = s."browserify-rsa@4.1.0";
        "browserify-rsa@^4.0.1" = s."browserify-rsa@4.1.0";
        "browserify-sign@4.2.1" = f "browserify-sign" "4.2.1" y "eaf4add46dd54be3bb3b36c0cf15abbeba7956c3" [
          (s."bn.js@^5.1.1")
          (s."browserify-rsa@^4.0.1")
          (s."create-hash@^1.2.0")
          (s."create-hmac@^1.1.7")
          (s."elliptic@^6.5.3")
          (s."inherits@^2.0.4")
          (s."parse-asn1@^5.1.5")
          (s."readable-stream@^3.6.0")
          (s."safe-buffer@^5.2.0")
          ];
        "browserify-sign@^4.0.0" = s."browserify-sign@4.2.1";
        "browserify-zlib@0.2.0" = f "browserify-zlib" "0.2.0" y "2869459d9aa3be245fe8fe2ca1f46e2e7f54d73f" [
          (s."pako@~1.0.5")
          ];
        "browserify-zlib@^0.2.0" = s."browserify-zlib@0.2.0";
        "browserslist@4.19.1" = f "browserslist" "4.19.1" y "4ac0435b35ab655896c31d53018b6dd5e9e4c9a3" [
          (s."caniuse-lite@^1.0.30001286")
          (s."electron-to-chromium@^1.4.17")
          (s."escalade@^3.1.1")
          (s."node-releases@^2.0.1")
          (s."picocolors@^1.0.0")
          ];
        "browserslist@^4.12.0" = s."browserslist@4.19.1";
        "browserslist@^4.17.5" = s."browserslist@4.19.1";
        "browserslist@^4.19.1" = s."browserslist@4.19.1";
        "bser@2.1.1" = f "bser" "2.1.1" y "e6787da20ece9d07998533cfd9de6f5c38f4bc05" [
          (s."node-int64@^0.4.0")
          ];
        "buffer-from@1.1.2" = f "buffer-from" "1.1.2" y "2b146a6fd72e80b4f55d255f35ed59a3a9a41bd5" [];
        "buffer-from@^1.0.0" = s."buffer-from@1.1.2";
        "buffer-xor@1.0.3" = f "buffer-xor" "1.0.3" y "26e61ed1422fb70dd42e6e36729ed51d855fe8d9" [];
        "buffer-xor@^1.0.3" = s."buffer-xor@1.0.3";
        "buffer@4.9.2" = f "buffer" "4.9.2" y "230ead344002988644841ab0244af8c44bbe3ef8" [
          (s."base64-js@^1.0.2")
          (s."ieee754@^1.1.4")
          (s."isarray@^1.0.0")
          ];
        "buffer@^4.3.0" = s."buffer@4.9.2";
        "builtin-status-codes@3.0.0" = f "builtin-status-codes" "3.0.0" y "85982878e21b98e1c66425e03d0174788f569ee8" [];
        "builtin-status-codes@^3.0.0" = s."builtin-status-codes@3.0.0";
        "bytes@3.0.0" = f "bytes" "3.0.0" y "d32815404d689699f85a4ea4fa8755dd13a96048" [];
        "bytes@3.1.1" = f "bytes" "3.1.1" y "3f018291cb4cbad9accb6e6970bca9c8889e879a" [];
        "c8@7.11.0" = f "c8" "7.11.0" y "b3ab4e9e03295a102c47ce11d4ef6d735d9a9ac9" [
          (s."@bcoe/v8-coverage@^0.2.3")
          (s."@istanbuljs/schema@^0.1.2")
          (s."find-up@^5.0.0")
          (s."foreground-child@^2.0.0")
          (s."istanbul-lib-coverage@^3.0.1")
          (s."istanbul-lib-report@^3.0.0")
          (s."istanbul-reports@^3.0.2")
          (s."rimraf@^3.0.0")
          (s."test-exclude@^6.0.0")
          (s."v8-to-istanbul@^8.0.0")
          (s."yargs@^16.2.0")
          (s."yargs-parser@^20.2.7")
          ];
        "c8@^7.6.0" = s."c8@7.11.0";
        "cacache@12.0.4" = f "cacache" "12.0.4" y "668bcbd105aeb5f1d92fe25570ec9525c8faa40c" [
          (s."bluebird@^3.5.5")
          (s."chownr@^1.1.1")
          (s."figgy-pudding@^3.5.1")
          (s."glob@^7.1.4")
          (s."graceful-fs@^4.1.15")
          (s."infer-owner@^1.0.3")
          (s."lru-cache@^5.1.1")
          (s."mississippi@^3.0.0")
          (s."mkdirp@^0.5.1")
          (s."move-concurrently@^1.0.1")
          (s."promise-inflight@^1.0.1")
          (s."rimraf@^2.6.3")
          (s."ssri@^6.0.1")
          (s."unique-filename@^1.1.1")
          (s."y18n@^4.0.0")
          ];
        "cacache@15.3.0" = f "cacache" "15.3.0" y "dc85380fb2f556fe3dda4c719bfa0ec875a7f1eb" [
          (s."@npmcli/fs@^1.0.0")
          (s."@npmcli/move-file@^1.0.1")
          (s."chownr@^2.0.0")
          (s."fs-minipass@^2.0.0")
          (s."glob@^7.1.4")
          (s."infer-owner@^1.0.4")
          (s."lru-cache@^6.0.0")
          (s."minipass@^3.1.1")
          (s."minipass-collect@^1.0.2")
          (s."minipass-flush@^1.0.5")
          (s."minipass-pipeline@^1.2.2")
          (s."mkdirp@^1.0.3")
          (s."p-map@^4.0.0")
          (s."promise-inflight@^1.0.1")
          (s."rimraf@^3.0.2")
          (s."ssri@^8.0.1")
          (s."tar@^6.0.2")
          (s."unique-filename@^1.1.1")
          ];
        "cacache@^12.0.2" = s."cacache@12.0.4";
        "cacache@^15.0.5" = s."cacache@15.3.0";
        "cache-base@1.0.1" = f "cache-base" "1.0.1" y "0a7f46416831c8b662ee36fe4e7c59d76f666ab2" [
          (s."collection-visit@^1.0.0")
          (s."component-emitter@^1.2.1")
          (s."get-value@^2.0.6")
          (s."has-value@^1.0.0")
          (s."isobject@^3.0.1")
          (s."set-value@^2.0.0")
          (s."to-object-path@^0.3.0")
          (s."union-value@^1.0.0")
          (s."unset-value@^1.0.0")
          ];
        "cache-base@^1.0.1" = s."cache-base@1.0.1";
        "call-bind@1.0.2" = f "call-bind" "1.0.2" y "b1d4e89e688119c3c9a903ad30abb2f6a919be3c" [
          (s."function-bind@^1.1.1")
          (s."get-intrinsic@^1.0.2")
          ];
        "call-bind@^1.0.0" = s."call-bind@1.0.2";
        "call-bind@^1.0.2" = s."call-bind@1.0.2";
        "call-me-maybe@1.0.1" = f "call-me-maybe" "1.0.1" y "26d208ea89e37b5cbde60250a15f031c16a4d66b" [];
        "call-me-maybe@^1.0.1" = s."call-me-maybe@1.0.1";
        "callsites@3.1.0" = f "callsites" "3.1.0" y "b3630abd8943432f54b3f0519238e33cd7df2f73" [];
        "callsites@^3.0.0" = s."callsites@3.1.0";
        "camel-case@4.1.2" = f "camel-case" "4.1.2" y "9728072a954f805228225a6deea6b38461e1bd5a" [
          (s."pascal-case@^3.1.2")
          (s."tslib@^2.0.3")
          ];
        "camel-case@^4.1.1" = s."camel-case@4.1.2";
        "camelcase-css@2.0.1" = f "camelcase-css" "2.0.1" y "ee978f6947914cc30c6b44741b6ed1df7f043fd5" [];
        "camelcase@5.3.1" = f "camelcase" "5.3.1" y "e3c9b31569e106811df242f715725a1f4c494320" [];
        "camelcase@6.3.0" = f "camelcase" "6.3.0" y "5685b95eb209ac9c0c177467778c9c84df58ba9a" [];
        "camelcase@^5.3.1" = s."camelcase@5.3.1";
        "camelcase@^6.2.0" = s."camelcase@6.3.0";
        "caniuse-lite@1.0.30001303" = f "caniuse-lite" "1.0.30001303" y "9b168e4f43ccfc372b86f4bc5a551d9b909c95c9" [];
        "caniuse-lite@^1.0.30001109" = s."caniuse-lite@1.0.30001303";
        "caniuse-lite@^1.0.30001286" = s."caniuse-lite@1.0.30001303";
        "capture-exit@2.0.0" = f "capture-exit" "2.0.0" y "fb953bfaebeb781f62898239dabb426d08a509a4" [
          (s."rsvp@^4.8.4")
          ];
        "capture-exit@^2.0.0" = s."capture-exit@2.0.0";
        "case-sensitive-paths-webpack-plugin@2.4.0" = f "case-sensitive-paths-webpack-plugin" "2.4.0" y "db64066c6422eed2e08cc14b986ca43796dbc6d4" [];
        "case-sensitive-paths-webpack-plugin@^2.3.0" = s."case-sensitive-paths-webpack-plugin@2.4.0";
        "ccount@1.1.0" = f "ccount" "1.1.0" y "246687debb6014735131be8abab2d93898f8d043" [];
        "ccount@^1.0.0" = s."ccount@1.1.0";
        "chalk@2.4.2" = f "chalk" "2.4.2" y "cd42541677a54333cf541a49108c1432b44c9424" [
          (s."ansi-styles@^3.2.1")
          (s."escape-string-regexp@^1.0.5")
          (s."supports-color@^5.3.0")
          ];
        "chalk@4.1.2" = f "chalk" "4.1.2" y "aac4e2b7734a740867aeb16bf02aad556a1e7a01" [
          (s."ansi-styles@^4.1.0")
          (s."supports-color@^7.1.0")
          ];
        "chalk@^2.0.0" = s."chalk@2.4.2";
        "chalk@^2.4.1" = s."chalk@2.4.2";
        "chalk@^4.0.0" = s."chalk@4.1.2";
        "chalk@^4.1.0" = s."chalk@4.1.2";
        "character-entities-legacy@1.1.4" = f "character-entities-legacy" "1.1.4" y "94bc1845dce70a5bb9d2ecc748725661293d8fc1" [];
        "character-entities-legacy@^1.0.0" = s."character-entities-legacy@1.1.4";
        "character-entities@1.2.4" = f "character-entities" "1.2.4" y "e12c3939b7eaf4e5b15e7ad4c5e28e1d48c5b16b" [];
        "character-entities@^1.0.0" = s."character-entities@1.2.4";
        "character-reference-invalid@1.1.4" = f "character-reference-invalid" "1.1.4" y "083329cda0eae272ab3dbbf37e9a382c13af1560" [];
        "character-reference-invalid@^1.0.0" = s."character-reference-invalid@1.1.4";
        "chokidar@2.1.8" = f "chokidar" "2.1.8" y "804b3a7b6a99358c3c5c61e71d8728f041cff917" [
          (s."anymatch@^2.0.0")
          (s."async-each@^1.0.1")
          (s."braces@^2.3.2")
          (s."glob-parent@^3.1.0")
          (s."inherits@^2.0.3")
          (s."is-binary-path@^1.0.0")
          (s."is-glob@^4.0.0")
          (s."normalize-path@^3.0.0")
          (s."path-is-absolute@^1.0.0")
          (s."readdirp@^2.2.1")
          (s."upath@^1.1.1")
          (s."fsevents@^1.2.7")
          ];
        "chokidar@3.5.3" = f "chokidar" "3.5.3" y "1cf37c8707b932bd1af1ae22c0432e2acd1903bd" [
          (s."anymatch@~3.1.2")
          (s."braces@~3.0.2")
          (s."glob-parent@~5.1.2")
          (s."is-binary-path@~2.1.0")
          (s."is-glob@~4.0.1")
          (s."normalize-path@~3.0.0")
          (s."readdirp@~3.6.0")
          (s."fsevents@~2.3.2")
          ];
        "chokidar@^2.1.8" = s."chokidar@2.1.8";
        "chokidar@^3.4.1" = s."chokidar@3.5.3";
        "chokidar@^3.4.2" = s."chokidar@3.5.3";
        "chownr@1.1.4" = f "chownr" "1.1.4" y "6fc9d7b42d32a583596337666e7d08084da2cc6b" [];
        "chownr@2.0.0" = f "chownr" "2.0.0" y "15bfbe53d2eab4cf70f18a8cd68ebe5b3cb1dece" [];
        "chownr@^1.1.1" = s."chownr@1.1.4";
        "chownr@^2.0.0" = s."chownr@2.0.0";
        "chrome-trace-event@1.0.3" = f "chrome-trace-event" "1.0.3" y "1015eced4741e15d06664a957dbbf50d041e26ac" [];
        "chrome-trace-event@^1.0.2" = s."chrome-trace-event@1.0.3";
        "ci-info@2.0.0" = f "ci-info" "2.0.0" y "67a9e964be31a51e15e5010d58e6f12834002f46" [];
        "ci-info@^2.0.0" = s."ci-info@2.0.0";
        "cipher-base@1.0.4" = f "cipher-base" "1.0.4" y "8760e4ecc272f4c363532f926d874aae2c1397de" [
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.0.1")
          ];
        "cipher-base@^1.0.0" = s."cipher-base@1.0.4";
        "cipher-base@^1.0.1" = s."cipher-base@1.0.4";
        "cipher-base@^1.0.3" = s."cipher-base@1.0.4";
        "class-utils@0.3.6" = f "class-utils" "0.3.6" y "f93369ae8b9a7ce02fd41faad0ca83033190c463" [
          (s."arr-union@^3.1.0")
          (s."define-property@^0.2.5")
          (s."isobject@^3.0.0")
          (s."static-extend@^0.1.1")
          ];
        "class-utils@^0.3.5" = s."class-utils@0.3.6";
        "clean-css@4.2.4" = f "clean-css" "4.2.4" y "733bf46eba4e607c6891ea57c24a989356831178" [
          (s."source-map@~0.6.0")
          ];
        "clean-css@^4.2.3" = s."clean-css@4.2.4";
        "clean-stack@2.2.0" = f "clean-stack" "2.2.0" y "ee8472dbb129e727b31e8a10a427dee9dfe4008b" [];
        "clean-stack@^2.0.0" = s."clean-stack@2.2.0";
        "cli-boxes@2.2.1" = f "cli-boxes" "2.2.1" y "ddd5035d25094fce220e9cab40a45840a440318f" [];
        "cli-boxes@^2.2.1" = s."cli-boxes@2.2.1";
        "cli-table3@0.6.1" = f "cli-table3" "0.6.1" y "36ce9b7af4847f288d3cdd081fbd09bf7bd237b8" [
          (s."string-width@^4.2.0")
          (s."colors@1.4.0")
          ];
        "cli-table3@^0.6.1" = s."cli-table3@0.6.1";
        "cliui@7.0.4" = f "cliui" "7.0.4" y "a0265ee655476fc807aea9df3df8df7783808b4f" [
          (s."string-width@^4.2.0")
          (s."strip-ansi@^6.0.0")
          (s."wrap-ansi@^7.0.0")
          ];
        "cliui@^7.0.2" = s."cliui@7.0.4";
        "clone-deep@4.0.1" = f "clone-deep" "4.0.1" y "c19fd9bdbbf85942b4fd979c84dcf7d5f07c2387" [
          (s."is-plain-object@^2.0.4")
          (s."kind-of@^6.0.2")
          (s."shallow-clone@^3.0.0")
          ];
        "clone-deep@^4.0.1" = s."clone-deep@4.0.1";
        "clsx@1.1.1" = f "clsx" "1.1.1" y "98b3134f9abbdf23b2663491ace13c5c03a73188" [];
        "clsx@^1.1.1" = s."clsx@1.1.1";
        "collapse-white-space@1.0.6" = f "collapse-white-space" "1.0.6" y "e63629c0016665792060dbbeb79c42239d2c5287" [];
        "collapse-white-space@^1.0.2" = s."collapse-white-space@1.0.6";
        "collection-visit@1.0.0" = f "collection-visit" "1.0.0" y "4bc0373c164bc3291b4d368c829cf1a80a59dca0" [
          (s."map-visit@^1.0.0")
          (s."object-visit@^1.0.0")
          ];
        "collection-visit@^1.0.0" = s."collection-visit@1.0.0";
        "color-convert@1.9.3" = f "color-convert" "1.9.3" y "bb71850690e1f136567de629d2d5471deda4c1e8" [
          (s."color-name@1.1.3")
          ];
        "color-convert@2.0.1" = f "color-convert" "2.0.1" y "72d3a68d598c9bdb3af2ad1e84f21d896abd4de3" [
          (s."color-name@~1.1.4")
          ];
        "color-convert@^1.9.0" = s."color-convert@1.9.3";
        "color-convert@^2.0.1" = s."color-convert@2.0.1";
        "color-name@1.1.3" = f "color-name" "1.1.3" y "a7d0558bd89c42f795dd42328f740831ca53bc25" [];
        "color-name@1.1.4" = f "color-name" "1.1.4" y "c2a09a87acbde69543de6f63fa3995c826c536a2" [];
        "color-name@~1.1.4" = s."color-name@1.1.4";
        "color-support@1.1.3" = f "color-support" "1.1.3" y "93834379a1cc9a0c61f82f52f0d04322251bd5a2" [];
        "color-support@^1.1.2" = s."color-support@1.1.3";
        "colors@1.4.0" = f "colors" "1.4.0" y "c50491479d4c1bdaed2c9ced32cf7c7dc2360f78" [];
        "combined-stream@1.0.8" = f "combined-stream" "1.0.8" y "c3d45a8b34fd730631a110a8a2520682b31d5a7f" [
          (s."delayed-stream@~1.0.0")
          ];
        "combined-stream@^1.0.8" = s."combined-stream@1.0.8";
        "comma-separated-tokens@1.0.8" = f "comma-separated-tokens" "1.0.8" y "632b80b6117867a158f1080ad498b2fbe7e3f5ea" [];
        "comma-separated-tokens@^1.0.0" = s."comma-separated-tokens@1.0.8";
        "commander@2.20.3" = f "commander" "2.20.3" y "fd485e84c03eb4881c20722ba48035e8531aeb33" [];
        "commander@4.1.1" = f "commander" "4.1.1" y "9fd602bd936294e9e9ef46a3f4d6964044b18068" [];
        "commander@6.2.1" = f "commander" "6.2.1" y "0792eb682dfbc325999bb2b84fddddba110ac73c" [];
        "commander@^2.19.0" = s."commander@2.20.3";
        "commander@^2.20.0" = s."commander@2.20.3";
        "commander@^4.1.1" = s."commander@4.1.1";
        "commander@^6.2.1" = s."commander@6.2.1";
        "common-path-prefix@3.0.0" = f "common-path-prefix" "3.0.0" y "7d007a7e07c58c4b4d5f433131a19141b29f11e0" [];
        "common-path-prefix@^3.0.0" = s."common-path-prefix@3.0.0";
        "commondir@1.0.1" = f "commondir" "1.0.1" y "ddd800da0c66127393cca5950ea968a3aaf1253b" [];
        "commondir@^1.0.1" = s."commondir@1.0.1";
        "component-emitter@1.3.0" = f "component-emitter" "1.3.0" y "16e4070fba8ae29b679f2215853ee181ab2eabc0" [];
        "component-emitter@^1.2.1" = s."component-emitter@1.3.0";
        "compressible@2.0.18" = f "compressible" "2.0.18" y "af53cca6b070d4c3c0750fbd77286a6d7cc46fba" [
          (s."mime-db@>= 1.43.0 < 2")
          ];
        "compressible@~2.0.16" = s."compressible@2.0.18";
        "compression@1.7.4" = f "compression" "1.7.4" y "95523eff170ca57c29a0ca41e6fe131f41e5bb8f" [
          (s."accepts@~1.3.5")
          (s."bytes@3.0.0")
          (s."compressible@~2.0.16")
          (s."debug@2.6.9")
          (s."on-headers@~1.0.2")
          (s."safe-buffer@5.1.2")
          (s."vary@~1.1.2")
          ];
        "compression@^1.7.4" = s."compression@1.7.4";
        "compute-scroll-into-view@1.0.17" = f "compute-scroll-into-view" "1.0.17" y "6a88f18acd9d42e9cf4baa6bec7e0522607ab7ab" [];
        "compute-scroll-into-view@^1.0.17" = s."compute-scroll-into-view@1.0.17";
        "concat-map@0.0.1" = f "concat-map" "0.0.1" y "d8a96bd77fd68df7793a73036a3ba0d5405d477b" [];
        "concat-stream@1.6.2" = f "concat-stream" "1.6.2" y "904bdf194cd3122fc675c77fc4ac3d4ff0fd1a34" [
          (s."buffer-from@^1.0.0")
          (s."inherits@^2.0.3")
          (s."readable-stream@^2.2.2")
          (s."typedarray@^0.0.6")
          ];
        "concat-stream@^1.5.0" = s."concat-stream@1.6.2";
        "console-browserify@1.2.0" = f "console-browserify" "1.2.0" y "67063cef57ceb6cf4993a2ab3a55840ae8c49336" [];
        "console-browserify@^1.1.0" = s."console-browserify@1.2.0";
        "console-control-strings@1.1.0" = f "console-control-strings" "1.1.0" y "3d7cf4464db6446ea644bf4b39507f9851008e8e" [];
        "console-control-strings@^1.0.0" = s."console-control-strings@1.1.0";
        "console-control-strings@^1.1.0" = s."console-control-strings@1.1.0";
        "constants-browserify@1.0.0" = f "constants-browserify" "1.0.0" y "c20b96d8c617748aaf1c16021760cd27fcb8cb75" [];
        "constants-browserify@^1.0.0" = s."constants-browserify@1.0.0";
        "content-disposition@0.5.4" = f "content-disposition" "0.5.4" y "8b82b4efac82512a02bb0b1dcec9d2c5e8eb5bfe" [
          (s."safe-buffer@5.2.1")
          ];
        "content-type@1.0.4" = f "content-type" "1.0.4" y "e138cc75e040c727b1966fe5e5f8c9aee256fe3b" [];
        "content-type@~1.0.4" = s."content-type@1.0.4";
        "convert-source-map@1.8.0" = f "convert-source-map" "1.8.0" y "f3373c32d21b4d780dd8004514684fb791ca4369" [
          (s."safe-buffer@~5.1.1")
          ];
        "convert-source-map@^1.4.0" = s."convert-source-map@1.8.0";
        "convert-source-map@^1.5.0" = s."convert-source-map@1.8.0";
        "convert-source-map@^1.6.0" = s."convert-source-map@1.8.0";
        "convert-source-map@^1.7.0" = s."convert-source-map@1.8.0";
        "cookie-signature@1.0.6" = f "cookie-signature" "1.0.6" y "e303a882b342cc3ee8ca513a79999734dab3ae2c" [];
        "cookie@0.4.1" = f "cookie" "0.4.1" y "afd713fe26ebd21ba95ceb61f9a8116e50a537d1" [];
        "copy-concurrently@1.0.5" = f "copy-concurrently" "1.0.5" y "92297398cae34937fcafd6ec8139c18051f0b5e0" [
          (s."aproba@^1.1.1")
          (s."fs-write-stream-atomic@^1.0.8")
          (s."iferr@^0.1.5")
          (s."mkdirp@^0.5.1")
          (s."rimraf@^2.5.4")
          (s."run-queue@^1.0.0")
          ];
        "copy-concurrently@^1.0.0" = s."copy-concurrently@1.0.5";
        "copy-descriptor@0.1.1" = f "copy-descriptor" "0.1.1" y "676f6eb3c39997c2ee1ac3a924fd6124748f578d" [];
        "copy-descriptor@^0.1.0" = s."copy-descriptor@0.1.1";
        "copy-to-clipboard@3.3.1" = f "copy-to-clipboard" "3.3.1" y "115aa1a9998ffab6196f93076ad6da3b913662ae" [
          (s."toggle-selection@^1.0.6")
          ];
        "copy-to-clipboard@^3.3.1" = s."copy-to-clipboard@3.3.1";
        "core-js-compat@3.20.3" = f "core-js-compat" "3.20.3" y "d71f85f94eb5e4bea3407412e549daa083d23bd6" [
          (s."browserslist@^4.19.1")
          (s."semver@7.0.0")
          ];
        "core-js-compat@^3.20.0" = s."core-js-compat@3.20.3";
        "core-js-compat@^3.20.2" = s."core-js-compat@3.20.3";
        "core-js-compat@^3.8.1" = s."core-js-compat@3.20.3";
        "core-js-pure@3.20.3" = f "core-js-pure" "3.20.3" y "6cc4f36da06c61d95254efc54024fe4797fd5d02" [];
        "core-js-pure@^3.8.1" = s."core-js-pure@3.20.3";
        "core-js-pure@^3.8.2" = s."core-js-pure@3.20.3";
        "core-js@3.20.3" = f "core-js" "3.20.3" y "c710d0a676e684522f3db4ee84e5e18a9d11d69a" [];
        "core-js@^3.0.4" = s."core-js@3.20.3";
        "core-js@^3.6.5" = s."core-js@3.20.3";
        "core-js@^3.8.2" = s."core-js@3.20.3";
        "core-util-is@1.0.3" = f "core-util-is" "1.0.3" y "a6042d3634c2b27e9328f837b965fac83808db85" [];
        "core-util-is@~1.0.0" = s."core-util-is@1.0.3";
        "cosmiconfig@6.0.0" = f "cosmiconfig" "6.0.0" y "da4fee853c52f6b1e6935f41c1a2fc50bd4a9982" [
          (s."@types/parse-json@^4.0.0")
          (s."import-fresh@^3.1.0")
          (s."parse-json@^5.0.0")
          (s."path-type@^4.0.0")
          (s."yaml@^1.7.2")
          ];
        "cosmiconfig@7.0.1" = f "cosmiconfig" "7.0.1" y "714d756522cace867867ccb4474c5d01bbae5d6d" [
          (s."@types/parse-json@^4.0.0")
          (s."import-fresh@^3.2.1")
          (s."parse-json@^5.0.0")
          (s."path-type@^4.0.0")
          (s."yaml@^1.10.0")
          ];
        "cosmiconfig@^6.0.0" = s."cosmiconfig@6.0.0";
        "cosmiconfig@^7.0.0" = s."cosmiconfig@7.0.1";
        "cp-file@7.0.0" = f "cp-file" "7.0.0" y "b9454cfd07fe3b974ab9ea0e5f29655791a9b8cd" [
          (s."graceful-fs@^4.1.2")
          (s."make-dir@^3.0.0")
          (s."nested-error-stacks@^2.0.0")
          (s."p-event@^4.1.0")
          ];
        "cp-file@^7.0.0" = s."cp-file@7.0.0";
        "cpy@8.1.2" = f "cpy" "8.1.2" y "e339ea54797ad23f8e3919a5cffd37bfc3f25935" [
          (s."arrify@^2.0.1")
          (s."cp-file@^7.0.0")
          (s."globby@^9.2.0")
          (s."has-glob@^1.0.0")
          (s."junk@^3.1.0")
          (s."nested-error-stacks@^2.1.0")
          (s."p-all@^2.1.0")
          (s."p-filter@^2.1.0")
          (s."p-map@^3.0.0")
          ];
        "cpy@^8.1.2" = s."cpy@8.1.2";
        "create-ecdh@4.0.4" = f "create-ecdh" "4.0.4" y "d6e7f4bffa66736085a0762fd3a632684dabcc4e" [
          (s."bn.js@^4.1.0")
          (s."elliptic@^6.5.3")
          ];
        "create-ecdh@^4.0.0" = s."create-ecdh@4.0.4";
        "create-hash@1.2.0" = f "create-hash" "1.2.0" y "889078af11a63756bcfb59bd221996be3a9ef196" [
          (s."cipher-base@^1.0.1")
          (s."inherits@^2.0.1")
          (s."md5.js@^1.3.4")
          (s."ripemd160@^2.0.1")
          (s."sha.js@^2.4.0")
          ];
        "create-hash@^1.1.0" = s."create-hash@1.2.0";
        "create-hash@^1.1.2" = s."create-hash@1.2.0";
        "create-hash@^1.2.0" = s."create-hash@1.2.0";
        "create-hmac@1.1.7" = f "create-hmac" "1.1.7" y "69170c78b3ab957147b2b8b04572e47ead2243ff" [
          (s."cipher-base@^1.0.3")
          (s."create-hash@^1.1.0")
          (s."inherits@^2.0.1")
          (s."ripemd160@^2.0.0")
          (s."safe-buffer@^5.0.1")
          (s."sha.js@^2.4.8")
          ];
        "create-hmac@^1.1.0" = s."create-hmac@1.1.7";
        "create-hmac@^1.1.4" = s."create-hmac@1.1.7";
        "create-hmac@^1.1.7" = s."create-hmac@1.1.7";
        "cross-spawn@6.0.5" = f "cross-spawn" "6.0.5" y "4a5ec7c64dfae22c3a14124dbacdee846d80cbc4" [
          (s."nice-try@^1.0.4")
          (s."path-key@^2.0.1")
          (s."semver@^5.5.0")
          (s."shebang-command@^1.2.0")
          (s."which@^1.2.9")
          ];
        "cross-spawn@7.0.3" = f "cross-spawn" "7.0.3" y "f73a85b9d5d41d045551c177e2882d4ac85728a6" [
          (s."path-key@^3.1.0")
          (s."shebang-command@^2.0.0")
          (s."which@^2.0.1")
          ];
        "cross-spawn@^6.0.0" = s."cross-spawn@6.0.5";
        "cross-spawn@^7.0.0" = s."cross-spawn@7.0.3";
        "crypto-browserify@3.12.0" = f "crypto-browserify" "3.12.0" y "396cf9f3137f03e4b8e532c58f698254e00f80ec" [
          (s."browserify-cipher@^1.0.0")
          (s."browserify-sign@^4.0.0")
          (s."create-ecdh@^4.0.0")
          (s."create-hash@^1.1.0")
          (s."create-hmac@^1.1.0")
          (s."diffie-hellman@^5.0.0")
          (s."inherits@^2.0.1")
          (s."pbkdf2@^3.0.3")
          (s."public-encrypt@^4.0.0")
          (s."randombytes@^2.0.0")
          (s."randomfill@^1.0.3")
          ];
        "crypto-browserify@^3.11.0" = s."crypto-browserify@3.12.0";
        "css-loader@3.6.0" = f "css-loader" "3.6.0" y "2e4b2c7e6e2d27f8c8f28f61bffcd2e6c91ef645" [
          (s."camelcase@^5.3.1")
          (s."cssesc@^3.0.0")
          (s."icss-utils@^4.1.1")
          (s."loader-utils@^1.2.3")
          (s."normalize-path@^3.0.0")
          (s."postcss@^7.0.32")
          (s."postcss-modules-extract-imports@^2.0.0")
          (s."postcss-modules-local-by-default@^3.0.2")
          (s."postcss-modules-scope@^2.2.0")
          (s."postcss-modules-values@^3.0.0")
          (s."postcss-value-parser@^4.1.0")
          (s."schema-utils@^2.7.0")
          (s."semver@^6.3.0")
          ];
        "css-loader@^3.6.0" = s."css-loader@3.6.0";
        "css-select@4.2.1" = f "css-select" "4.2.1" y "9e665d6ae4c7f9d65dbe69d0316e3221fb274cdd" [
          (s."boolbase@^1.0.0")
          (s."css-what@^5.1.0")
          (s."domhandler@^4.3.0")
          (s."domutils@^2.8.0")
          (s."nth-check@^2.0.1")
          ];
        "css-select@^4.1.3" = s."css-select@4.2.1";
        "css-what@5.1.0" = f "css-what" "5.1.0" y "3f7b707aadf633baf62c2ceb8579b545bb40f7fe" [];
        "css-what@^5.1.0" = s."css-what@5.1.0";
        "cssesc@3.0.0" = f "cssesc" "3.0.0" y "37741919903b868565e1c09ea747445cd18983ee" [];
        "cssesc@^3.0.0" = s."cssesc@3.0.0";
        "csstype@2.6.19" = f "csstype" "2.6.19" y "feeb5aae89020bb389e1f63669a5ed490e391caa" [];
        "csstype@3.0.10" = f "csstype" "3.0.10" y "2ad3a7bed70f35b965707c092e5f30b327c290e5" [];
        "csstype@^2.5.7" = s."csstype@2.6.19";
        "csstype@^3.0.2" = s."csstype@3.0.10";
        "cyclist@1.0.1" = f "cyclist" "1.0.1" y "596e9698fd0c80e12038c2b82d6eb1b35b6224d9" [];
        "cyclist@^1.0.1" = s."cyclist@1.0.1";
        "debug@2.6.9" = f "debug" "2.6.9" y "5d128515df134ff327e90a4c93f4e077a536341f" [
          (s."ms@2.0.0")
          ];
        "debug@3.2.7" = f "debug" "3.2.7" y "72580b7e9145fb39b6676f9c5e5fb100b934179a" [
          (s."ms@^2.1.1")
          ];
        "debug@4.3.3" = f "debug" "4.3.3" y "04266e0b70a98d4462e6e288e38259213332b664" [
          (s."ms@2.1.2")
          ];
        "debug@^2.2.0" = s."debug@2.6.9";
        "debug@^2.3.3" = s."debug@2.6.9";
        "debug@^2.6.0" = s."debug@2.6.9";
        "debug@^3.0.0" = s."debug@3.2.7";
        "debug@^4.1.0" = s."debug@4.3.3";
        "debug@^4.1.1" = s."debug@4.3.3";
        "decode-uri-component@0.2.0" = f "decode-uri-component" "0.2.0" y "eb3913333458775cb84cd1a1fae062106bb87545" [];
        "decode-uri-component@^0.2.0" = s."decode-uri-component@0.2.0";
        "dedent@0.7.0" = f "dedent" "0.7.0" y "2495ddbaf6eb874abb0e1be9df22d2e5a544326c" [];
        "dedent@^0.7.0" = s."dedent@0.7.0";
        "deep-is@0.1.4" = f "deep-is" "0.1.4" y "a6f2dce612fadd2ef1f519b73551f17e85199831" [];
        "deep-is@~0.1.3" = s."deep-is@0.1.4";
        "deep-object-diff@1.1.7" = f "deep-object-diff" "1.1.7" y "348b3246f426427dd633eaa50e1ed1fc2eafc7e4" [];
        "deep-object-diff@^1.1.0" = s."deep-object-diff@1.1.7";
        "deepmerge@4.2.2" = f "deepmerge" "4.2.2" y "44d2ea3679b8f4d4ffba33f03d865fc1e7bf4955" [];
        "deepmerge@^4.2.2" = s."deepmerge@4.2.2";
        "define-properties@1.1.3" = f "define-properties" "1.1.3" y "cf88da6cbee26fe6db7094f61d870cbd84cee9f1" [
          (s."object-keys@^1.0.12")
          ];
        "define-properties@^1.1.2" = s."define-properties@1.1.3";
        "define-properties@^1.1.3" = s."define-properties@1.1.3";
        "define-property@0.2.5" = f "define-property" "0.2.5" y "c35b1ef918ec3c990f9a5bc57be04aacec5c8116" [
          (s."is-descriptor@^0.1.0")
          ];
        "define-property@1.0.0" = f "define-property" "1.0.0" y "769ebaaf3f4a63aad3af9e8d304c9bbe79bfb0e6" [
          (s."is-descriptor@^1.0.0")
          ];
        "define-property@2.0.2" = f "define-property" "2.0.2" y "d459689e8d654ba77e02a817f8710d702cb16e9d" [
          (s."is-descriptor@^1.0.2")
          (s."isobject@^3.0.1")
          ];
        "define-property@^0.2.5" = s."define-property@0.2.5";
        "define-property@^1.0.0" = s."define-property@1.0.0";
        "define-property@^2.0.2" = s."define-property@2.0.2";
        "delayed-stream@1.0.0" = f "delayed-stream" "1.0.0" y "df3ae199acadfb7d440aaae0b29e2272b24ec619" [];
        "delayed-stream@~1.0.0" = s."delayed-stream@1.0.0";
        "delegates@1.0.0" = f "delegates" "1.0.0" y "84c6e159b81904fdca59a0ef44cd870d31250f9a" [];
        "delegates@^1.0.0" = s."delegates@1.0.0";
        "depd@1.1.2" = f "depd" "1.1.2" y "9bcd52e14c097763e749b274c4346ed2e560b5a9" [];
        "depd@~1.1.2" = s."depd@1.1.2";
        "des.js@1.0.1" = f "des.js" "1.0.1" y "5382142e1bdc53f85d86d53e5f4aa7deb91e0843" [
          (s."inherits@^2.0.1")
          (s."minimalistic-assert@^1.0.0")
          ];
        "des.js@^1.0.0" = s."des.js@1.0.1";
        "destroy@1.0.4" = f "destroy" "1.0.4" y "978857442c44749e4206613e37946205826abd80" [];
        "destroy@~1.0.4" = s."destroy@1.0.4";
        "detab@2.0.4" = f "detab" "2.0.4" y "b927892069aff405fbb9a186fe97a44a92a94b43" [
          (s."repeat-string@^1.5.4")
          ];
        "detect-port@1.3.0" = f "detect-port" "1.3.0" y "d9c40e9accadd4df5cac6a782aefd014d573d1f1" [
          (s."address@^1.0.1")
          (s."debug@^2.6.0")
          ];
        "detect-port@^1.3.0" = s."detect-port@1.3.0";
        "diffie-hellman@5.0.3" = f "diffie-hellman" "5.0.3" y "40e8ee98f55a2149607146921c63e1ae5f3d2875" [
          (s."bn.js@^4.1.0")
          (s."miller-rabin@^4.0.0")
          (s."randombytes@^2.0.0")
          ];
        "diffie-hellman@^5.0.0" = s."diffie-hellman@5.0.3";
        "dir-glob@2.2.2" = f "dir-glob" "2.2.2" y "fa09f0694153c8918b18ba0deafae94769fc50c4" [
          (s."path-type@^3.0.0")
          ];
        "dir-glob@3.0.1" = f "dir-glob" "3.0.1" y "56dbf73d992a4a93ba1584f4534063fd2e41717f" [
          (s."path-type@^4.0.0")
          ];
        "dir-glob@^2.2.2" = s."dir-glob@2.2.2";
        "dir-glob@^3.0.1" = s."dir-glob@3.0.1";
        "doctrine@3.0.0" = f "doctrine" "3.0.0" y "addebead72a6574db783639dc87a121773973961" [
          (s."esutils@^2.0.2")
          ];
        "doctrine@^3.0.0" = s."doctrine@3.0.0";
        "dom-converter@0.2.0" = f "dom-converter" "0.2.0" y "6721a9daee2e293682955b6afe416771627bb768" [
          (s."utila@~0.4")
          ];
        "dom-converter@^0.2.0" = s."dom-converter@0.2.0";
        "dom-serializer@1.3.2" = f "dom-serializer" "1.3.2" y "6206437d32ceefaec7161803230c7a20bc1b4d91" [
          (s."domelementtype@^2.0.1")
          (s."domhandler@^4.2.0")
          (s."entities@^2.0.0")
          ];
        "dom-serializer@^1.0.1" = s."dom-serializer@1.3.2";
        "dom-walk@0.1.2" = f "dom-walk" "0.1.2" y "0c548bef048f4d1f2a97249002236060daa3fd84" [];
        "dom-walk@^0.1.0" = s."dom-walk@0.1.2";
        "domain-browser@1.2.0" = f "domain-browser" "1.2.0" y "3d31f50191a6749dd1375a7f522e823d42e54eda" [];
        "domain-browser@^1.1.1" = s."domain-browser@1.2.0";
        "domelementtype@2.2.0" = f "domelementtype" "2.2.0" y "9a0b6c2782ed6a1c7323d42267183df9bd8b1d57" [];
        "domelementtype@^2.0.1" = s."domelementtype@2.2.0";
        "domelementtype@^2.2.0" = s."domelementtype@2.2.0";
        "domhandler@4.3.0" = f "domhandler" "4.3.0" y "16c658c626cf966967e306f966b431f77d4a5626" [
          (s."domelementtype@^2.2.0")
          ];
        "domhandler@^4.0.0" = s."domhandler@4.3.0";
        "domhandler@^4.2.0" = s."domhandler@4.3.0";
        "domhandler@^4.3.0" = s."domhandler@4.3.0";
        "domutils@2.8.0" = f "domutils" "2.8.0" y "4437def5db6e2d1f5d6ee859bd95ca7d02048135" [
          (s."dom-serializer@^1.0.1")
          (s."domelementtype@^2.2.0")
          (s."domhandler@^4.2.0")
          ];
        "domutils@^2.5.2" = s."domutils@2.8.0";
        "domutils@^2.8.0" = s."domutils@2.8.0";
        "dot-case@3.0.4" = f "dot-case" "3.0.4" y "9b2b670d00a431667a8a75ba29cd1b98809ce751" [
          (s."no-case@^3.0.4")
          (s."tslib@^2.0.3")
          ];
        "dot-case@^3.0.4" = s."dot-case@3.0.4";
        "dotenv-expand@5.1.0" = f "dotenv-expand" "5.1.0" y "3fbaf020bfd794884072ea26b1e9791d45a629f0" [];
        "dotenv-expand@^5.1.0" = s."dotenv-expand@5.1.0";
        "dotenv@8.6.0" = f "dotenv" "8.6.0" y "061af664d19f7f4d8fc6e4ff9b584ce237adcb8b" [];
        "dotenv@^8.0.0" = s."dotenv@8.6.0";
        "downshift@6.1.7" = f "downshift" "6.1.7" y "fdb4c4e4f1d11587985cd76e21e8b4b3fa72e44c" [
          (s."@babel/runtime@^7.14.8")
          (s."compute-scroll-into-view@^1.0.17")
          (s."prop-types@^15.7.2")
          (s."react-is@^17.0.2")
          (s."tslib@^2.3.0")
          ];
        "downshift@^6.0.15" = s."downshift@6.1.7";
        "duplexify@3.7.1" = f "duplexify" "3.7.1" y "2a4df5317f6ccfd91f86d6fd25d8d8a103b88309" [
          (s."end-of-stream@^1.0.0")
          (s."inherits@^2.0.1")
          (s."readable-stream@^2.0.0")
          (s."stream-shift@^1.0.0")
          ];
        "duplexify@^3.4.2" = s."duplexify@3.7.1";
        "duplexify@^3.6.0" = s."duplexify@3.7.1";
        "ee-first@1.1.1" = f "ee-first" "1.1.1" y "590c61156b0ae2f4f0255732a158b266bc56b21d" [];
        "electron-to-chromium@1.4.54" = f "electron-to-chromium" "1.4.54" y "69005d39ed11542e1bcb65ec1a98e44d39527ba8" [];
        "electron-to-chromium@^1.4.17" = s."electron-to-chromium@1.4.54";
        "element-resize-detector@1.2.4" = f "element-resize-detector" "1.2.4" y "3e6c5982dd77508b5fa7e6d5c02170e26325c9b1" [
          (s."batch-processor@1.0.0")
          ];
        "element-resize-detector@^1.2.2" = s."element-resize-detector@1.2.4";
        "elliptic@6.5.4" = f "elliptic" "6.5.4" y "da37cebd31e79a1367e941b592ed1fbebd58abbb" [
          (s."bn.js@^4.11.9")
          (s."brorand@^1.1.0")
          (s."hash.js@^1.0.0")
          (s."hmac-drbg@^1.0.1")
          (s."inherits@^2.0.4")
          (s."minimalistic-assert@^1.0.1")
          (s."minimalistic-crypto-utils@^1.0.1")
          ];
        "elliptic@^6.5.3" = s."elliptic@6.5.4";
        "emoji-regex@8.0.0" = f "emoji-regex" "8.0.0" y "e818fd69ce5ccfcb404594f842963bf53164cc37" [];
        "emoji-regex@^8.0.0" = s."emoji-regex@8.0.0";
        "emojis-list@3.0.0" = f "emojis-list" "3.0.0" y "5570662046ad29e2e916e71aae260abdff4f6a78" [];
        "emojis-list@^3.0.0" = s."emojis-list@3.0.0";
        "emotion-theming@10.3.0" = f "emotion-theming" "10.3.0" y "7f84d7099581d7ffe808aab5cd870e30843db72a" [
          (s."@babel/runtime@^7.5.5")
          (s."@emotion/weak-memoize@0.2.5")
          (s."hoist-non-react-statics@^3.3.0")
          ];
        "emotion-theming@^10.0.27" = s."emotion-theming@10.3.0";
        "encodeurl@1.0.2" = f "encodeurl" "1.0.2" y "ad3ff4c86ec2d029322f5a02c3a9a606c95b3f59" [];
        "encodeurl@~1.0.2" = s."encodeurl@1.0.2";
        "end-of-stream@1.4.4" = f "end-of-stream" "1.4.4" y "5ae64a5f45057baf3626ec14da0ca5e4b2431eb0" [
          (s."once@^1.4.0")
          ];
        "end-of-stream@^1.0.0" = s."end-of-stream@1.4.4";
        "end-of-stream@^1.1.0" = s."end-of-stream@1.4.4";
        "endent@2.1.0" = f "endent" "2.1.0" y "5aaba698fb569e5e18e69e1ff7a28ff35373cd88" [
          (s."dedent@^0.7.0")
          (s."fast-json-parse@^1.0.3")
          (s."objectorarray@^1.0.5")
          ];
        "endent@^2.0.1" = s."endent@2.1.0";
        "enhanced-resolve@4.5.0" = f "enhanced-resolve" "4.5.0" y "2f3cfd84dbe3b487f18f2db2ef1e064a571ca5ec" [
          (s."graceful-fs@^4.1.2")
          (s."memory-fs@^0.5.0")
          (s."tapable@^1.0.0")
          ];
        "enhanced-resolve@^4.5.0" = s."enhanced-resolve@4.5.0";
        "entities@2.2.0" = f "entities" "2.2.0" y "098dc90ebb83d8dffa089d55256b351d34c4da55" [];
        "entities@^2.0.0" = s."entities@2.2.0";
        "errno@0.1.8" = f "errno" "0.1.8" y "8bb3e9c7d463be4976ff888f76b4809ebc2e811f" [
          (s."prr@~1.0.1")
          ];
        "errno@^0.1.3" = s."errno@0.1.8";
        "errno@~0.1.7" = s."errno@0.1.8";
        "error-ex@1.3.2" = f "error-ex" "1.3.2" y "b4ac40648107fdcdcfae242f428bea8a14d4f1bf" [
          (s."is-arrayish@^0.2.1")
          ];
        "error-ex@^1.3.1" = s."error-ex@1.3.2";
        "error-stack-parser@2.0.6" = f "error-stack-parser" "2.0.6" y "5a99a707bd7a4c58a797902d48d82803ede6aad8" [
          (s."stackframe@^1.1.1")
          ];
        "error-stack-parser@^2.0.6" = s."error-stack-parser@2.0.6";
        "es-abstract@1.19.1" = f "es-abstract" "1.19.1" y "d4885796876916959de78edaa0df456627115ec3" [
          (s."call-bind@^1.0.2")
          (s."es-to-primitive@^1.2.1")
          (s."function-bind@^1.1.1")
          (s."get-intrinsic@^1.1.1")
          (s."get-symbol-description@^1.0.0")
          (s."has@^1.0.3")
          (s."has-symbols@^1.0.2")
          (s."internal-slot@^1.0.3")
          (s."is-callable@^1.2.4")
          (s."is-negative-zero@^2.0.1")
          (s."is-regex@^1.1.4")
          (s."is-shared-array-buffer@^1.0.1")
          (s."is-string@^1.0.7")
          (s."is-weakref@^1.0.1")
          (s."object-inspect@^1.11.0")
          (s."object-keys@^1.1.1")
          (s."object.assign@^4.1.2")
          (s."string.prototype.trimend@^1.0.4")
          (s."string.prototype.trimstart@^1.0.4")
          (s."unbox-primitive@^1.0.1")
          ];
        "es-abstract@^1.19.0" = s."es-abstract@1.19.1";
        "es-abstract@^1.19.1" = s."es-abstract@1.19.1";
        "es-array-method-boxes-properly@1.0.0" = f "es-array-method-boxes-properly" "1.0.0" y "873f3e84418de4ee19c5be752990b2e44718d09e" [];
        "es-array-method-boxes-properly@^1.0.0" = s."es-array-method-boxes-properly@1.0.0";
        "es-get-iterator@1.1.2" = f "es-get-iterator" "1.1.2" y "9234c54aba713486d7ebde0220864af5e2b283f7" [
          (s."call-bind@^1.0.2")
          (s."get-intrinsic@^1.1.0")
          (s."has-symbols@^1.0.1")
          (s."is-arguments@^1.1.0")
          (s."is-map@^2.0.2")
          (s."is-set@^2.0.2")
          (s."is-string@^1.0.5")
          (s."isarray@^2.0.5")
          ];
        "es-get-iterator@^1.0.2" = s."es-get-iterator@1.1.2";
        "es-to-primitive@1.2.1" = f "es-to-primitive" "1.2.1" y "e55cd4c9cdc188bcefb03b366c736323fc5c898a" [
          (s."is-callable@^1.1.4")
          (s."is-date-object@^1.0.1")
          (s."is-symbol@^1.0.2")
          ];
        "es-to-primitive@^1.2.1" = s."es-to-primitive@1.2.1";
        "es5-shim@4.6.4" = f "es5-shim" "4.6.4" y "10ce5f06c7bccfdd60b4e08edf95c7e2fbc1dc2a" [];
        "es5-shim@^4.5.13" = s."es5-shim@4.6.4";
        "es6-shim@0.35.6" = f "es6-shim" "0.35.6" y "d10578301a83af2de58b9eadb7c2c9945f7388a0" [];
        "es6-shim@^0.35.5" = s."es6-shim@0.35.6";
        "escalade@3.1.1" = f "escalade" "3.1.1" y "d8cfdc7000965c5a0174b4a82eaa5c0552742e40" [];
        "escalade@^3.1.1" = s."escalade@3.1.1";
        "escape-html@1.0.3" = f "escape-html" "1.0.3" y "0258eae4d3d0c0974de1c169188ef0051d1d1988" [];
        "escape-html@~1.0.3" = s."escape-html@1.0.3";
        "escape-string-regexp@1.0.5" = f "escape-string-regexp" "1.0.5" y "1b61c0562190a8dff6ae3bb2cf0200ca130b86d4" [];
        "escape-string-regexp@^1.0.5" = s."escape-string-regexp@1.0.5";
        "escodegen@2.0.0" = f "escodegen" "2.0.0" y "5e32b12833e8aa8fa35e1bf0befa89380484c7dd" [
          (s."esprima@^4.0.1")
          (s."estraverse@^5.2.0")
          (s."esutils@^2.0.2")
          (s."optionator@^0.8.1")
          (s."source-map@~0.6.1")
          ];
        "escodegen@^2.0.0" = s."escodegen@2.0.0";
        "eslint-scope@4.0.3" = f "eslint-scope" "4.0.3" y "ca03833310f6889a3264781aa82e63eb9cfe7848" [
          (s."esrecurse@^4.1.0")
          (s."estraverse@^4.1.1")
          ];
        "eslint-scope@^4.0.3" = s."eslint-scope@4.0.3";
        "esprima@4.0.1" = f "esprima" "4.0.1" y "13b04cdb3e6c5d19df91ab6987a8695619b0aa71" [];
        "esprima@^4.0.0" = s."esprima@4.0.1";
        "esprima@^4.0.1" = s."esprima@4.0.1";
        "esrecurse@4.3.0" = f "esrecurse" "4.3.0" y "7ad7964d679abb28bee72cec63758b1c5d2c9921" [
          (s."estraverse@^5.2.0")
          ];
        "esrecurse@^4.1.0" = s."esrecurse@4.3.0";
        "estraverse@4.3.0" = f "estraverse" "4.3.0" y "398ad3f3c5a24948be7725e83d11a7de28cdbd1d" [];
        "estraverse@5.3.0" = f "estraverse" "5.3.0" y "2eea5290702f26ab8fe5370370ff86c965d21123" [];
        "estraverse@^4.1.1" = s."estraverse@4.3.0";
        "estraverse@^5.2.0" = s."estraverse@5.3.0";
        "estree-to-babel@3.2.1" = f "estree-to-babel" "3.2.1" y "82e78315275c3ca74475fdc8ac1a5103c8a75bf5" [
          (s."@babel/traverse@^7.1.6")
          (s."@babel/types@^7.2.0")
          (s."c8@^7.6.0")
          ];
        "estree-to-babel@^3.1.0" = s."estree-to-babel@3.2.1";
        "esutils@2.0.3" = f "esutils" "2.0.3" y "74d2eb4de0b8da1293711910d50775b9b710ef64" [];
        "esutils@^2.0.2" = s."esutils@2.0.3";
        "etag@1.8.1" = f "etag" "1.8.1" y "41ae2eeb65efa62268aebfea83ac7d79299b0887" [];
        "etag@~1.8.1" = s."etag@1.8.1";
        "events@3.3.0" = f "events" "3.3.0" y "31a95ad0a924e2d2c419a813aeb2c4e878ea7400" [];
        "events@^3.0.0" = s."events@3.3.0";
        "evp_bytestokey@1.0.3" = f "evp_bytestokey" "1.0.3" y "7fcbdb198dc71959432efe13842684e0525acb02" [
          (s."md5.js@^1.3.4")
          (s."safe-buffer@^5.1.1")
          ];
        "evp_bytestokey@^1.0.0" = s."evp_bytestokey@1.0.3";
        "evp_bytestokey@^1.0.3" = s."evp_bytestokey@1.0.3";
        "exec-sh@0.3.6" = f "exec-sh" "0.3.6" y "ff264f9e325519a60cb5e273692943483cca63bc" [];
        "exec-sh@^0.3.2" = s."exec-sh@0.3.6";
        "execa@1.0.0" = f "execa" "1.0.0" y "c6236a5bb4df6d6f15e88e7f017798216749ddd8" [
          (s."cross-spawn@^6.0.0")
          (s."get-stream@^4.0.0")
          (s."is-stream@^1.1.0")
          (s."npm-run-path@^2.0.0")
          (s."p-finally@^1.0.0")
          (s."signal-exit@^3.0.0")
          (s."strip-eof@^1.0.0")
          ];
        "execa@^1.0.0" = s."execa@1.0.0";
        "expand-brackets@2.1.4" = f "expand-brackets" "2.1.4" y "b77735e315ce30f6b6eff0f83b04151a22449622" [
          (s."debug@^2.3.3")
          (s."define-property@^0.2.5")
          (s."extend-shallow@^2.0.1")
          (s."posix-character-classes@^0.1.0")
          (s."regex-not@^1.0.0")
          (s."snapdragon@^0.8.1")
          (s."to-regex@^3.0.1")
          ];
        "expand-brackets@^2.1.4" = s."expand-brackets@2.1.4";
        "express@4.17.2" = f "express" "4.17.2" y "c18369f265297319beed4e5558753cc8c1364cb3" [
          (s."accepts@~1.3.7")
          (s."array-flatten@1.1.1")
          (s."body-parser@1.19.1")
          (s."content-disposition@0.5.4")
          (s."content-type@~1.0.4")
          (s."cookie@0.4.1")
          (s."cookie-signature@1.0.6")
          (s."debug@2.6.9")
          (s."depd@~1.1.2")
          (s."encodeurl@~1.0.2")
          (s."escape-html@~1.0.3")
          (s."etag@~1.8.1")
          (s."finalhandler@~1.1.2")
          (s."fresh@0.5.2")
          (s."merge-descriptors@1.0.1")
          (s."methods@~1.1.2")
          (s."on-finished@~2.3.0")
          (s."parseurl@~1.3.3")
          (s."path-to-regexp@0.1.7")
          (s."proxy-addr@~2.0.7")
          (s."qs@6.9.6")
          (s."range-parser@~1.2.1")
          (s."safe-buffer@5.2.1")
          (s."send@0.17.2")
          (s."serve-static@1.14.2")
          (s."setprototypeof@1.2.0")
          (s."statuses@~1.5.0")
          (s."type-is@~1.6.18")
          (s."utils-merge@1.0.1")
          (s."vary@~1.1.2")
          ];
        "express@^4.17.1" = s."express@4.17.2";
        "extend-shallow@2.0.1" = f "extend-shallow" "2.0.1" y "51af7d614ad9a9f610ea1bafbb989d6b1c56890f" [
          (s."is-extendable@^0.1.0")
          ];
        "extend-shallow@3.0.2" = f "extend-shallow" "3.0.2" y "26a71aaf073b39fb2127172746131c2704028db8" [
          (s."assign-symbols@^1.0.0")
          (s."is-extendable@^1.0.1")
          ];
        "extend-shallow@^2.0.1" = s."extend-shallow@2.0.1";
        "extend-shallow@^3.0.0" = s."extend-shallow@3.0.2";
        "extend-shallow@^3.0.2" = s."extend-shallow@3.0.2";
        "extend@3.0.2" = f "extend" "3.0.2" y "f8b1136b4071fbd8eb140aff858b1019ec2915fa" [];
        "extend@^3.0.0" = s."extend@3.0.2";
        "extglob@2.0.4" = f "extglob" "2.0.4" y "ad00fe4dc612a9232e8718711dc5cb5ab0285543" [
          (s."array-unique@^0.3.2")
          (s."define-property@^1.0.0")
          (s."expand-brackets@^2.1.4")
          (s."extend-shallow@^2.0.1")
          (s."fragment-cache@^0.2.1")
          (s."regex-not@^1.0.0")
          (s."snapdragon@^0.8.1")
          (s."to-regex@^3.0.1")
          ];
        "extglob@^2.0.4" = s."extglob@2.0.4";
        "fast-deep-equal@3.1.3" = f "fast-deep-equal" "3.1.3" y "3a7d56b559d6cbc3eb512325244e619a65c6c525" [];
        "fast-deep-equal@^3.1.1" = s."fast-deep-equal@3.1.3";
        "fast-deep-equal@^3.1.3" = s."fast-deep-equal@3.1.3";
        "fast-glob@2.2.7" = f "fast-glob" "2.2.7" y "6953857c3afa475fff92ee6015d52da70a4cd39d" [
          (s."@mrmlnc/readdir-enhanced@^2.2.1")
          (s."@nodelib/fs.stat@^1.1.2")
          (s."glob-parent@^3.1.0")
          (s."is-glob@^4.0.0")
          (s."merge2@^1.2.3")
          (s."micromatch@^3.1.10")
          ];
        "fast-glob@3.2.11" = f "fast-glob" "3.2.11" y "a1172ad95ceb8a16e20caa5c5e56480e5129c1d9" [
          (s."@nodelib/fs.stat@^2.0.2")
          (s."@nodelib/fs.walk@^1.2.3")
          (s."glob-parent@^5.1.2")
          (s."merge2@^1.3.0")
          (s."micromatch@^4.0.4")
          ];
        "fast-glob@^2.2.6" = s."fast-glob@2.2.7";
        "fast-glob@^3.2.9" = s."fast-glob@3.2.11";
        "fast-json-parse@1.0.3" = f "fast-json-parse" "1.0.3" y "43e5c61ee4efa9265633046b770fb682a7577c4d" [];
        "fast-json-parse@^1.0.3" = s."fast-json-parse@1.0.3";
        "fast-json-stable-stringify@2.1.0" = f "fast-json-stable-stringify" "2.1.0" y "874bf69c6f404c2b5d99c481341399fd55892633" [];
        "fast-json-stable-stringify@^2.0.0" = s."fast-json-stable-stringify@2.1.0";
        "fast-levenshtein@2.0.6" = f "fast-levenshtein" "2.0.6" y "3d8a5c66883a16a30ca8643e851f19baa7797917" [];
        "fast-levenshtein@~2.0.6" = s."fast-levenshtein@2.0.6";
        "fastq@1.13.0" = f "fastq" "1.13.0" y "616760f88a7526bdfc596b7cab8c18938c36b98c" [
          (s."reusify@^1.0.4")
          ];
        "fastq@^1.6.0" = s."fastq@1.13.0";
        "fault@1.0.4" = f "fault" "1.0.4" y "eafcfc0a6d214fc94601e170df29954a4f842f13" [
          (s."format@^0.2.0")
          ];
        "fault@^1.0.0" = s."fault@1.0.4";
        "fb-watchman@2.0.1" = f "fb-watchman" "2.0.1" y "fc84fb39d2709cf3ff6d743706157bb5708a8a85" [
          (s."bser@2.1.1")
          ];
        "fb-watchman@^2.0.0" = s."fb-watchman@2.0.1";
        "figgy-pudding@3.5.2" = f "figgy-pudding" "3.5.2" y "b4eee8148abb01dcf1d1ac34367d59e12fa61d6e" [];
        "figgy-pudding@^3.5.1" = s."figgy-pudding@3.5.2";
        "file-loader@6.2.0" = f "file-loader" "6.2.0" y "baef7cf8e1840df325e4390b4484879480eebe4d" [
          (s."loader-utils@^2.0.0")
          (s."schema-utils@^3.0.0")
          ];
        "file-loader@^6.2.0" = s."file-loader@6.2.0";
        "file-system-cache@1.0.5" = f "file-system-cache" "1.0.5" y "84259b36a2bbb8d3d6eb1021d3132ffe64cfff4f" [
          (s."bluebird@^3.3.5")
          (s."fs-extra@^0.30.0")
          (s."ramda@^0.21.0")
          ];
        "file-system-cache@^1.0.5" = s."file-system-cache@1.0.5";
        "file-uri-to-path@1.0.0" = f "file-uri-to-path" "1.0.0" y "553a7b8446ff6f684359c445f1e37a05dacc33dd" [];
        "fill-range@4.0.0" = f "fill-range" "4.0.0" y "d544811d428f98eb06a63dc402d2403c328c38f7" [
          (s."extend-shallow@^2.0.1")
          (s."is-number@^3.0.0")
          (s."repeat-string@^1.6.1")
          (s."to-regex-range@^2.1.0")
          ];
        "fill-range@7.0.1" = f "fill-range" "7.0.1" y "1919a6a7c75fe38b2c7c77e5198535da9acdda40" [
          (s."to-regex-range@^5.0.1")
          ];
        "fill-range@^4.0.0" = s."fill-range@4.0.0";
        "fill-range@^7.0.1" = s."fill-range@7.0.1";
        "finalhandler@1.1.2" = f "finalhandler" "1.1.2" y "b7e7d000ffd11938d0fdb053506f6ebabe9f587d" [
          (s."debug@2.6.9")
          (s."encodeurl@~1.0.2")
          (s."escape-html@~1.0.3")
          (s."on-finished@~2.3.0")
          (s."parseurl@~1.3.3")
          (s."statuses@~1.5.0")
          (s."unpipe@~1.0.0")
          ];
        "finalhandler@~1.1.2" = s."finalhandler@1.1.2";
        "find-cache-dir@2.1.0" = f "find-cache-dir" "2.1.0" y "8d0f94cd13fe43c6c7c261a0d86115ca918c05f7" [
          (s."commondir@^1.0.1")
          (s."make-dir@^2.0.0")
          (s."pkg-dir@^3.0.0")
          ];
        "find-cache-dir@3.3.2" = f "find-cache-dir" "3.3.2" y "b30c5b6eff0730731aea9bbd9dbecbd80256d64b" [
          (s."commondir@^1.0.1")
          (s."make-dir@^3.0.2")
          (s."pkg-dir@^4.1.0")
          ];
        "find-cache-dir@^2.0.0" = s."find-cache-dir@2.1.0";
        "find-cache-dir@^2.1.0" = s."find-cache-dir@2.1.0";
        "find-cache-dir@^3.3.1" = s."find-cache-dir@3.3.2";
        "find-root@1.1.0" = f "find-root" "1.1.0" y "abcfc8ba76f708c42a97b3d685b7e9450bfb9ce4" [];
        "find-root@^1.1.0" = s."find-root@1.1.0";
        "find-up@3.0.0" = f "find-up" "3.0.0" y "49169f1d7993430646da61ecc5ae355c21c97b73" [
          (s."locate-path@^3.0.0")
          ];
        "find-up@4.1.0" = f "find-up" "4.1.0" y "97afe7d6cdc0bc5928584b7c8d7b16e8a9aa5d19" [
          (s."locate-path@^5.0.0")
          (s."path-exists@^4.0.0")
          ];
        "find-up@5.0.0" = f "find-up" "5.0.0" y "4c92819ecb7083561e4f4a240a86be5198f536fc" [
          (s."locate-path@^6.0.0")
          (s."path-exists@^4.0.0")
          ];
        "find-up@^3.0.0" = s."find-up@3.0.0";
        "find-up@^4.0.0" = s."find-up@4.1.0";
        "find-up@^4.1.0" = s."find-up@4.1.0";
        "find-up@^5.0.0" = s."find-up@5.0.0";
        "flat-cache@3.0.4" = f "flat-cache" "3.0.4" y "61b0338302b2fe9f957dcc32fc2a87f1c3048b11" [
          (s."flatted@^3.1.0")
          (s."rimraf@^3.0.2")
          ];
        "flat-cache@^3.0.4" = s."flat-cache@3.0.4";
        "flatted@3.2.5" = f "flatted" "3.2.5" y "76c8584f4fc843db64702a6bd04ab7a8bd666da3" [];
        "flatted@^3.1.0" = s."flatted@3.2.5";
        "flush-write-stream@1.1.1" = f "flush-write-stream" "1.1.1" y "8dd7d873a1babc207d94ead0c2e0e44276ebf2e8" [
          (s."inherits@^2.0.3")
          (s."readable-stream@^2.3.6")
          ];
        "flush-write-stream@^1.0.0" = s."flush-write-stream@1.1.1";
        "for-in@1.0.2" = f "for-in" "1.0.2" y "81068d295a8142ec0ac726c6e2200c30fb6d5e80" [];
        "for-in@^1.0.2" = s."for-in@1.0.2";
        "foreground-child@2.0.0" = f "foreground-child" "2.0.0" y "71b32800c9f15aa8f2f83f4a6bd9bff35d861a53" [
          (s."cross-spawn@^7.0.0")
          (s."signal-exit@^3.0.2")
          ];
        "foreground-child@^2.0.0" = s."foreground-child@2.0.0";
        "fork-ts-checker-webpack-plugin@4.1.6" = f "fork-ts-checker-webpack-plugin" "4.1.6" y "5055c703febcf37fa06405d400c122b905167fc5" [
          (s."@babel/code-frame@^7.5.5")
          (s."chalk@^2.4.1")
          (s."micromatch@^3.1.10")
          (s."minimatch@^3.0.4")
          (s."semver@^5.6.0")
          (s."tapable@^1.0.0")
          (s."worker-rpc@^0.1.0")
          ];
        "fork-ts-checker-webpack-plugin@6.5.0" = f "fork-ts-checker-webpack-plugin" "6.5.0" y "0282b335fa495a97e167f69018f566ea7d2a2b5e" [
          (s."@babel/code-frame@^7.8.3")
          (s."@types/json-schema@^7.0.5")
          (s."chalk@^4.1.0")
          (s."chokidar@^3.4.2")
          (s."cosmiconfig@^6.0.0")
          (s."deepmerge@^4.2.2")
          (s."fs-extra@^9.0.0")
          (s."glob@^7.1.6")
          (s."memfs@^3.1.2")
          (s."minimatch@^3.0.4")
          (s."schema-utils@2.7.0")
          (s."semver@^7.3.2")
          (s."tapable@^1.0.0")
          ];
        "fork-ts-checker-webpack-plugin@^4.1.6" = s."fork-ts-checker-webpack-plugin@4.1.6";
        "fork-ts-checker-webpack-plugin@^6.0.4" = s."fork-ts-checker-webpack-plugin@6.5.0";
        "form-data@3.0.1" = f "form-data" "3.0.1" y "ebd53791b78356a99af9a300d4282c4d5eb9755f" [
          (s."asynckit@^0.4.0")
          (s."combined-stream@^1.0.8")
          (s."mime-types@^2.1.12")
          ];
        "form-data@^3.0.0" = s."form-data@3.0.1";
        "format@0.2.2" = f "format" "0.2.2" y "d6170107e9efdc4ed30c9dc39016df942b5cb58b" [];
        "format@^0.2.0" = s."format@0.2.2";
        "forwarded@0.2.0" = f "forwarded" "0.2.0" y "2269936428aad4c15c7ebe9779a84bf0b2a81811" [];
        "fragment-cache@0.2.1" = f "fragment-cache" "0.2.1" y "4290fad27f13e89be7f33799c6bc5a0abfff0d19" [
          (s."map-cache@^0.2.2")
          ];
        "fragment-cache@^0.2.1" = s."fragment-cache@0.2.1";
        "fresh@0.5.2" = f "fresh" "0.5.2" y "3d8cadd90d976569fa835ab1f8e4b23a105605a7" [];
        "from2@2.3.0" = f "from2" "2.3.0" y "8bfb5502bde4a4d36cfdeea007fcca21d7e382af" [
          (s."inherits@^2.0.1")
          (s."readable-stream@^2.0.0")
          ];
        "from2@^2.1.0" = s."from2@2.3.0";
        "fs-extra@0.30.0" = f "fs-extra" "0.30.0" y "f233ffcc08d4da7d432daa449776989db1df93f0" [
          (s."graceful-fs@^4.1.2")
          (s."jsonfile@^2.1.0")
          (s."klaw@^1.0.0")
          (s."path-is-absolute@^1.0.0")
          (s."rimraf@^2.2.8")
          ];
        "fs-extra@9.1.0" = f "fs-extra" "9.1.0" y "5954460c764a8da2094ba3554bf839e6b9a7c86d" [
          (s."at-least-node@^1.0.0")
          (s."graceful-fs@^4.2.0")
          (s."jsonfile@^6.0.1")
          (s."universalify@^2.0.0")
          ];
        "fs-extra@^0.30.0" = s."fs-extra@0.30.0";
        "fs-extra@^9.0.0" = s."fs-extra@9.1.0";
        "fs-extra@^9.0.1" = s."fs-extra@9.1.0";
        "fs-minipass@2.1.0" = f "fs-minipass" "2.1.0" y "7f5036fdbf12c63c169190cbe4199c852271f9fb" [
          (s."minipass@^3.0.0")
          ];
        "fs-minipass@^2.0.0" = s."fs-minipass@2.1.0";
        "fs-monkey@1.0.3" = f "fs-monkey" "1.0.3" y "ae3ac92d53bb328efe0e9a1d9541f6ad8d48e2d3" [];
        "fs-write-stream-atomic@1.0.10" = f "fs-write-stream-atomic" "1.0.10" y "b47df53493ef911df75731e70a9ded0189db40c9" [
          (s."graceful-fs@^4.1.2")
          (s."iferr@^0.1.5")
          (s."imurmurhash@^0.1.4")
          (s."readable-stream@1 || 2")
          ];
        "fs-write-stream-atomic@^1.0.8" = s."fs-write-stream-atomic@1.0.10";
        "fs.realpath@1.0.0" = f "fs.realpath" "1.0.0" y "1504ad2523158caa40db4a2787cb01411994ea4f" [];
        "fs.realpath@^1.0.0" = s."fs.realpath@1.0.0";
        "fsevents@1.2.13" = f "fsevents" "1.2.13" y "f325cb0455592428bcf11b383370ef70e3bfcc38" [
          (s."bindings@^1.5.0")
          (s."nan@^2.12.1")
          ];
        "fsevents@2.3.2" = f "fsevents" "2.3.2" y "8a526f78b8fdf4623b709e0b975c52c24c02fd1a" [];
        "fsevents@^1.2.7" = s."fsevents@1.2.13";
        "fsevents@^2.1.2" = s."fsevents@2.3.2";
        "fsevents@~2.3.2" = s."fsevents@2.3.2";
        "function-bind@1.1.1" = f "function-bind" "1.1.1" y "a56899d3ea3c9bab874bb9773b7c5ede92f4895d" [];
        "function-bind@^1.1.1" = s."function-bind@1.1.1";
        "function.prototype.name@1.1.5" = f "function.prototype.name" "1.1.5" y "cce0505fe1ffb80503e6f9e46cc64e46a12a9621" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.0")
          (s."functions-have-names@^1.2.2")
          ];
        "function.prototype.name@^1.1.0" = s."function.prototype.name@1.1.5";
        "functions-have-names@1.2.2" = f "functions-have-names" "1.2.2" y "98d93991c39da9361f8e50b337c4f6e41f120e21" [];
        "functions-have-names@^1.2.2" = s."functions-have-names@1.2.2";
        "fuse.js@3.6.1" = f "fuse.js" "3.6.1" y "7de85fdd6e1b3377c23ce010892656385fd9b10c" [];
        "fuse.js@^3.6.1" = s."fuse.js@3.6.1";
        "gauge@3.0.2" = f "gauge" "3.0.2" y "03bf4441c044383908bcfa0656ad91803259b395" [
          (s."aproba@^1.0.3 || ^2.0.0")
          (s."color-support@^1.1.2")
          (s."console-control-strings@^1.0.0")
          (s."has-unicode@^2.0.1")
          (s."object-assign@^4.1.1")
          (s."signal-exit@^3.0.0")
          (s."string-width@^4.2.3")
          (s."strip-ansi@^6.0.1")
          (s."wide-align@^1.1.2")
          ];
        "gauge@^3.0.0" = s."gauge@3.0.2";
        "gensync@1.0.0-beta.2" = f "gensync" "1.0.0-beta.2" y "32a6ee76c3d7f52d46b2b1ae5d93fea8580a25e0" [];
        "gensync@^1.0.0-beta.1" = s."gensync@1.0.0-beta.2";
        "gensync@^1.0.0-beta.2" = s."gensync@1.0.0-beta.2";
        "get-caller-file@2.0.5" = f "get-caller-file" "2.0.5" y "4f94412a82db32f36e3b0b9741f8a97feb031f7e" [];
        "get-caller-file@^2.0.5" = s."get-caller-file@2.0.5";
        "get-intrinsic@1.1.1" = f "get-intrinsic" "1.1.1" y "15f59f376f855c446963948f0d24cd3637b4abc6" [
          (s."function-bind@^1.1.1")
          (s."has@^1.0.3")
          (s."has-symbols@^1.0.1")
          ];
        "get-intrinsic@^1.0.2" = s."get-intrinsic@1.1.1";
        "get-intrinsic@^1.1.0" = s."get-intrinsic@1.1.1";
        "get-intrinsic@^1.1.1" = s."get-intrinsic@1.1.1";
        "get-package-type@0.1.0" = f "get-package-type" "0.1.0" y "8de2d803cff44df3bc6c456e6668b36c3926e11a" [];
        "get-package-type@^0.1.0" = s."get-package-type@0.1.0";
        "get-stream@4.1.0" = f "get-stream" "4.1.0" y "c1b255575f3dc21d59bfc79cd3d2b46b1c3a54b5" [
          (s."pump@^3.0.0")
          ];
        "get-stream@^4.0.0" = s."get-stream@4.1.0";
        "get-symbol-description@1.0.0" = f "get-symbol-description" "1.0.0" y "7fdb81c900101fbd564dd5f1a30af5aadc1e58d6" [
          (s."call-bind@^1.0.2")
          (s."get-intrinsic@^1.1.1")
          ];
        "get-symbol-description@^1.0.0" = s."get-symbol-description@1.0.0";
        "get-value@2.0.6" = f "get-value" "2.0.6" y "dc15ca1c672387ca76bd37ac0a395ba2042a2c28" [];
        "get-value@^2.0.3" = s."get-value@2.0.6";
        "get-value@^2.0.6" = s."get-value@2.0.6";
        "github-slugger@1.4.0" = f "github-slugger" "1.4.0" y "206eb96cdb22ee56fdc53a28d5a302338463444e" [];
        "github-slugger@^1.0.0" = s."github-slugger@1.4.0";
        "glob-parent@3.1.0" = f "glob-parent" "3.1.0" y "9e6af6299d8d3bd2bd40430832bd113df906c5ae" [
          (s."is-glob@^3.1.0")
          (s."path-dirname@^1.0.0")
          ];
        "glob-parent@5.1.2" = f "glob-parent" "5.1.2" y "869832c58034fe68a4093c17dc15e8340d8401c4" [
          (s."is-glob@^4.0.1")
          ];
        "glob-parent@^3.1.0" = s."glob-parent@3.1.0";
        "glob-parent@^5.1.2" = s."glob-parent@5.1.2";
        "glob-parent@~5.1.2" = s."glob-parent@5.1.2";
        "glob-promise@3.4.0" = f "glob-promise" "3.4.0" y "b6b8f084504216f702dc2ce8c9bc9ac8866fdb20" [
          (s."@types/glob@*")
          ];
        "glob-promise@^3.4.0" = s."glob-promise@3.4.0";
        "glob-to-regexp@0.3.0" = f "glob-to-regexp" "0.3.0" y "8c5a1494d2066c570cc3bfe4496175acc4d502ab" [];
        "glob-to-regexp@0.4.1" = f "glob-to-regexp" "0.4.1" y "c75297087c851b9a578bd217dd59a92f59fe546e" [];
        "glob-to-regexp@^0.3.0" = s."glob-to-regexp@0.3.0";
        "glob-to-regexp@^0.4.1" = s."glob-to-regexp@0.4.1";
        "glob@7.2.0" = f "glob" "7.2.0" y "d15535af7732e02e948f4c41628bd910293f6023" [
          (s."fs.realpath@^1.0.0")
          (s."inflight@^1.0.4")
          (s."inherits@2")
          (s."minimatch@^3.0.4")
          (s."once@^1.3.0")
          (s."path-is-absolute@^1.0.0")
          ];
        "glob@^7.1.3" = s."glob@7.2.0";
        "glob@^7.1.4" = s."glob@7.2.0";
        "glob@^7.1.6" = s."glob@7.2.0";
        "global@4.4.0" = f "global" "4.4.0" y "3e7b105179006a323ed71aafca3e9c57a5cc6406" [
          (s."min-document@^2.19.0")
          (s."process@^0.11.10")
          ];
        "global@^4.4.0" = s."global@4.4.0";
        "globals@11.12.0" = f "globals" "11.12.0" y "ab8795338868a0babd8525758018c2a7eb95c42e" [];
        "globals@^11.1.0" = s."globals@11.12.0";
        "globalthis@1.0.2" = f "globalthis" "1.0.2" y "2a235d34f4d8036219f7e34929b5de9e18166b8b" [
          (s."define-properties@^1.1.3")
          ];
        "globalthis@^1.0.0" = s."globalthis@1.0.2";
        "globby@11.1.0" = f "globby" "11.1.0" y "bd4be98bb042f83d796f7e3811991fbe82a0d34b" [
          (s."array-union@^2.1.0")
          (s."dir-glob@^3.0.1")
          (s."fast-glob@^3.2.9")
          (s."ignore@^5.2.0")
          (s."merge2@^1.4.1")
          (s."slash@^3.0.0")
          ];
        "globby@9.2.0" = f "globby" "9.2.0" y "fd029a706c703d29bdd170f4b6db3a3f7a7cb63d" [
          (s."@types/glob@^7.1.1")
          (s."array-union@^1.0.2")
          (s."dir-glob@^2.2.2")
          (s."fast-glob@^2.2.6")
          (s."glob@^7.1.3")
          (s."ignore@^4.0.3")
          (s."pify@^4.0.1")
          (s."slash@^2.0.0")
          ];
        "globby@^11.0.2" = s."globby@11.1.0";
        "globby@^9.2.0" = s."globby@9.2.0";
        "graceful-fs@4.2.9" = f "graceful-fs" "4.2.9" y "041b05df45755e587a24942279b9d113146e1c96" [];
        "graceful-fs@^4.1.11" = s."graceful-fs@4.2.9";
        "graceful-fs@^4.1.15" = s."graceful-fs@4.2.9";
        "graceful-fs@^4.1.2" = s."graceful-fs@4.2.9";
        "graceful-fs@^4.1.6" = s."graceful-fs@4.2.9";
        "graceful-fs@^4.1.9" = s."graceful-fs@4.2.9";
        "graceful-fs@^4.2.0" = s."graceful-fs@4.2.9";
        "graceful-fs@^4.2.4" = s."graceful-fs@4.2.9";
        "handlebars@4.7.7" = f "handlebars" "4.7.7" y "9ce33416aad02dbd6c8fafa8240d5d98004945a1" [
          (s."minimist@^1.2.5")
          (s."neo-async@^2.6.0")
          (s."source-map@^0.6.1")
          (s."wordwrap@^1.0.0")
          (s."uglify-js@^3.1.4")
          ];
        "handlebars@^4.7.7" = s."handlebars@4.7.7";
        "has-bigints@1.0.1" = f "has-bigints" "1.0.1" y "64fe6acb020673e3b78db035a5af69aa9d07b113" [];
        "has-bigints@^1.0.1" = s."has-bigints@1.0.1";
        "has-flag@3.0.0" = f "has-flag" "3.0.0" y "b5d454dc2199ae225699f3467e5a07f3b955bafd" [];
        "has-flag@4.0.0" = f "has-flag" "4.0.0" y "944771fd9c81c81265c4d6941860da06bb59479b" [];
        "has-flag@^3.0.0" = s."has-flag@3.0.0";
        "has-flag@^4.0.0" = s."has-flag@4.0.0";
        "has-glob@1.0.0" = f "has-glob" "1.0.0" y "9aaa9eedbffb1ba3990a7b0010fb678ee0081207" [
          (s."is-glob@^3.0.0")
          ];
        "has-glob@^1.0.0" = s."has-glob@1.0.0";
        "has-symbols@1.0.2" = f "has-symbols" "1.0.2" y "165d3070c00309752a1236a479331e3ac56f1423" [];
        "has-symbols@^1.0.1" = s."has-symbols@1.0.2";
        "has-symbols@^1.0.2" = s."has-symbols@1.0.2";
        "has-tostringtag@1.0.0" = f "has-tostringtag" "1.0.0" y "7e133818a7d394734f941e73c3d3f9291e658b25" [
          (s."has-symbols@^1.0.2")
          ];
        "has-tostringtag@^1.0.0" = s."has-tostringtag@1.0.0";
        "has-unicode@2.0.1" = f "has-unicode" "2.0.1" y "e0e6fe6a28cf51138855e086d1691e771de2a8b9" [];
        "has-unicode@^2.0.1" = s."has-unicode@2.0.1";
        "has-value@0.3.1" = f "has-value" "0.3.1" y "7b1f58bada62ca827ec0a2078025654845995e1f" [
          (s."get-value@^2.0.3")
          (s."has-values@^0.1.4")
          (s."isobject@^2.0.0")
          ];
        "has-value@1.0.0" = f "has-value" "1.0.0" y "18b281da585b1c5c51def24c930ed29a0be6b177" [
          (s."get-value@^2.0.6")
          (s."has-values@^1.0.0")
          (s."isobject@^3.0.0")
          ];
        "has-value@^0.3.1" = s."has-value@0.3.1";
        "has-value@^1.0.0" = s."has-value@1.0.0";
        "has-values@0.1.4" = f "has-values" "0.1.4" y "6d61de95d91dfca9b9a02089ad384bff8f62b771" [];
        "has-values@1.0.0" = f "has-values" "1.0.0" y "95b0b63fec2146619a6fe57fe75628d5a39efe4f" [
          (s."is-number@^3.0.0")
          (s."kind-of@^4.0.0")
          ];
        "has-values@^0.1.4" = s."has-values@0.1.4";
        "has-values@^1.0.0" = s."has-values@1.0.0";
        "has@1.0.3" = f "has" "1.0.3" y "722d7cbfc1f6aa8241f16dd814e011e1f41e8796" [
          (s."function-bind@^1.1.1")
          ];
        "has@^1.0.3" = s."has@1.0.3";
        "hash-base@3.1.0" = f "hash-base" "3.1.0" y "55c381d9e06e1d2997a883b4a3fddfe7f0d3af33" [
          (s."inherits@^2.0.4")
          (s."readable-stream@^3.6.0")
          (s."safe-buffer@^5.2.0")
          ];
        "hash-base@^3.0.0" = s."hash-base@3.1.0";
        "hash.js@1.1.7" = f "hash.js" "1.1.7" y "0babca538e8d4ee4a0f8988d68866537a003cf42" [
          (s."inherits@^2.0.3")
          (s."minimalistic-assert@^1.0.1")
          ];
        "hash.js@^1.0.0" = s."hash.js@1.1.7";
        "hash.js@^1.0.3" = s."hash.js@1.1.7";
        "hast-to-hyperscript@9.0.1" = f "hast-to-hyperscript" "9.0.1" y "9b67fd188e4c81e8ad66f803855334173920218d" [
          (s."@types/unist@^2.0.3")
          (s."comma-separated-tokens@^1.0.0")
          (s."property-information@^5.3.0")
          (s."space-separated-tokens@^1.0.0")
          (s."style-to-object@^0.3.0")
          (s."unist-util-is@^4.0.0")
          (s."web-namespaces@^1.0.0")
          ];
        "hast-to-hyperscript@^9.0.0" = s."hast-to-hyperscript@9.0.1";
        "hast-util-from-parse5@6.0.1" = f "hast-util-from-parse5" "6.0.1" y "554e34abdeea25ac76f5bd950a1f0180e0b3bc2a" [
          (s."@types/parse5@^5.0.0")
          (s."hastscript@^6.0.0")
          (s."property-information@^5.0.0")
          (s."vfile@^4.0.0")
          (s."vfile-location@^3.2.0")
          (s."web-namespaces@^1.0.0")
          ];
        "hast-util-from-parse5@^6.0.0" = s."hast-util-from-parse5@6.0.1";
        "hast-util-parse-selector@2.2.5" = f "hast-util-parse-selector" "2.2.5" y "d57c23f4da16ae3c63b3b6ca4616683313499c3a" [];
        "hast-util-parse-selector@^2.0.0" = s."hast-util-parse-selector@2.2.5";
        "hast-util-raw@6.0.1" = f "hast-util-raw" "6.0.1" y "973b15930b7529a7b66984c98148b46526885977" [
          (s."@types/hast@^2.0.0")
          (s."hast-util-from-parse5@^6.0.0")
          (s."hast-util-to-parse5@^6.0.0")
          (s."html-void-elements@^1.0.0")
          (s."parse5@^6.0.0")
          (s."unist-util-position@^3.0.0")
          (s."vfile@^4.0.0")
          (s."web-namespaces@^1.0.0")
          (s."xtend@^4.0.0")
          (s."zwitch@^1.0.0")
          ];
        "hast-util-to-parse5@6.0.0" = f "hast-util-to-parse5" "6.0.0" y "1ec44650b631d72952066cea9b1445df699f8479" [
          (s."hast-to-hyperscript@^9.0.0")
          (s."property-information@^5.0.0")
          (s."web-namespaces@^1.0.0")
          (s."xtend@^4.0.0")
          (s."zwitch@^1.0.0")
          ];
        "hast-util-to-parse5@^6.0.0" = s."hast-util-to-parse5@6.0.0";
        "hastscript@6.0.0" = f "hastscript" "6.0.0" y "e8768d7eac56c3fdeac8a92830d58e811e5bf640" [
          (s."@types/hast@^2.0.0")
          (s."comma-separated-tokens@^1.0.0")
          (s."hast-util-parse-selector@^2.0.0")
          (s."property-information@^5.0.0")
          (s."space-separated-tokens@^1.0.0")
          ];
        "hastscript@^6.0.0" = s."hastscript@6.0.0";
        "he@1.2.0" = f "he" "1.2.0" y "84ae65fa7eafb165fddb61566ae14baf05664f0f" [];
        "he@^1.2.0" = s."he@1.2.0";
        "highlight.js@10.7.3" = f "highlight.js" "10.7.3" y "697272e3991356e40c3cac566a74eef681756531" [];
        "highlight.js@^10.1.1" = s."highlight.js@10.7.3";
        "highlight.js@~10.7.0" = s."highlight.js@10.7.3";
        "history@5.0.0" = f "history" "5.0.0" y "0cabbb6c4bbf835addb874f8259f6d25101efd08" [
          (s."@babel/runtime@^7.7.6")
          ];
        "history@5.2.0" = f "history" "5.2.0" y "7cdd31cf9bac3c5d31f09c231c9928fad0007b7c" [
          (s."@babel/runtime@^7.7.6")
          ];
        "history@^5.2.0" = s."history@5.2.0";
        "hmac-drbg@1.0.1" = f "hmac-drbg" "1.0.1" y "d2745701025a6c775a6c545793ed502fc0c649a1" [
          (s."hash.js@^1.0.3")
          (s."minimalistic-assert@^1.0.0")
          (s."minimalistic-crypto-utils@^1.0.1")
          ];
        "hmac-drbg@^1.0.1" = s."hmac-drbg@1.0.1";
        "hoist-non-react-statics@3.3.2" = f "hoist-non-react-statics" "3.3.2" y "ece0acaf71d62c2969c2ec59feff42a4b1a85b45" [
          (s."react-is@^16.7.0")
          ];
        "hoist-non-react-statics@^3.3.0" = s."hoist-non-react-statics@3.3.2";
        "hosted-git-info@2.8.9" = f "hosted-git-info" "2.8.9" y "dffc0bf9a21c02209090f2aa69429e1414daf3f9" [];
        "hosted-git-info@^2.1.4" = s."hosted-git-info@2.8.9";
        "html-entities@2.3.2" = f "html-entities" "2.3.2" y "760b404685cb1d794e4f4b744332e3b00dcfe488" [];
        "html-entities@^2.1.0" = s."html-entities@2.3.2";
        "html-escaper@2.0.2" = f "html-escaper" "2.0.2" y "dfd60027da36a36dfcbe236262c00a5822681453" [];
        "html-escaper@^2.0.0" = s."html-escaper@2.0.2";
        "html-minifier-terser@5.1.1" = f "html-minifier-terser" "5.1.1" y "922e96f1f3bb60832c2634b79884096389b1f054" [
          (s."camel-case@^4.1.1")
          (s."clean-css@^4.2.3")
          (s."commander@^4.1.1")
          (s."he@^1.2.0")
          (s."param-case@^3.0.3")
          (s."relateurl@^0.2.7")
          (s."terser@^4.6.3")
          ];
        "html-minifier-terser@^5.0.1" = s."html-minifier-terser@5.1.1";
        "html-tags@3.1.0" = f "html-tags" "3.1.0" y "7b5e6f7e665e9fb41f30007ed9e0d41e97fb2140" [];
        "html-tags@^3.1.0" = s."html-tags@3.1.0";
        "html-void-elements@1.0.5" = f "html-void-elements" "1.0.5" y "ce9159494e86d95e45795b166c2021c2cfca4483" [];
        "html-void-elements@^1.0.0" = s."html-void-elements@1.0.5";
        "html-webpack-plugin@4.5.2" = f "html-webpack-plugin" "4.5.2" y "76fc83fa1a0f12dd5f7da0404a54e2699666bc12" [
          (s."@types/html-minifier-terser@^5.0.0")
          (s."@types/tapable@^1.0.5")
          (s."@types/webpack@^4.41.8")
          (s."html-minifier-terser@^5.0.1")
          (s."loader-utils@^1.2.3")
          (s."lodash@^4.17.20")
          (s."pretty-error@^2.1.1")
          (s."tapable@^1.1.3")
          (s."util.promisify@1.0.0")
          ];
        "html-webpack-plugin@^4.0.0" = s."html-webpack-plugin@4.5.2";
        "htmlparser2@6.1.0" = f "htmlparser2" "6.1.0" y "c4d762b6c3371a05dbe65e94ae43a9f845fb8fb7" [
          (s."domelementtype@^2.0.1")
          (s."domhandler@^4.0.0")
          (s."domutils@^2.5.2")
          (s."entities@^2.0.0")
          ];
        "htmlparser2@^6.1.0" = s."htmlparser2@6.1.0";
        "http-errors@1.8.1" = f "http-errors" "1.8.1" y "7c3f28577cbc8a207388455dbd62295ed07bd68c" [
          (s."depd@~1.1.2")
          (s."inherits@2.0.4")
          (s."setprototypeof@1.2.0")
          (s."statuses@>= 1.5.0 < 2")
          (s."toidentifier@1.0.1")
          ];
        "https-browserify@1.0.0" = f "https-browserify" "1.0.0" y "ec06c10e0a34c0f2faf199f7fd7fc78fffd03c73" [];
        "https-browserify@^1.0.0" = s."https-browserify@1.0.0";
        "iconv-lite@0.4.24" = f "iconv-lite" "0.4.24" y "2022b4b25fbddc21d2f524974a474aafe733908b" [
          (s."safer-buffer@>= 2.1.2 < 3")
          ];
        "icss-utils@4.1.1" = f "icss-utils" "4.1.1" y "21170b53789ee27447c2f47dd683081403f9a467" [
          (s."postcss@^7.0.14")
          ];
        "icss-utils@^4.0.0" = s."icss-utils@4.1.1";
        "icss-utils@^4.1.1" = s."icss-utils@4.1.1";
        "ieee754@1.2.1" = f "ieee754" "1.2.1" y "8eb7a10a63fff25d15a57b001586d177d1b0d352" [];
        "ieee754@^1.1.4" = s."ieee754@1.2.1";
        "iferr@0.1.5" = f "iferr" "0.1.5" y "c60eed69e6d8fdb6b3104a1fcbca1c192dc5b501" [];
        "iferr@^0.1.5" = s."iferr@0.1.5";
        "ignore@4.0.6" = f "ignore" "4.0.6" y "750e3db5862087b4737ebac8207ffd1ef27b25fc" [];
        "ignore@5.2.0" = f "ignore" "5.2.0" y "6d3bac8fa7fe0d45d9f9be7bac2fc279577e345a" [];
        "ignore@^4.0.3" = s."ignore@4.0.6";
        "ignore@^5.2.0" = s."ignore@5.2.0";
        "import-fresh@3.3.0" = f "import-fresh" "3.3.0" y "37162c25fcb9ebaa2e6e53d5b4d88ce17d9e0c2b" [
          (s."parent-module@^1.0.0")
          (s."resolve-from@^4.0.0")
          ];
        "import-fresh@^3.1.0" = s."import-fresh@3.3.0";
        "import-fresh@^3.2.1" = s."import-fresh@3.3.0";
        "imurmurhash@0.1.4" = f "imurmurhash" "0.1.4" y "9218b9b2b928a238b13dc4fb6b6d576f231453ea" [];
        "imurmurhash@^0.1.4" = s."imurmurhash@0.1.4";
        "indent-string@4.0.0" = f "indent-string" "4.0.0" y "624f8f4497d619b2d9768531d58f4122854d7251" [];
        "indent-string@^4.0.0" = s."indent-string@4.0.0";
        "infer-owner@1.0.4" = f "infer-owner" "1.0.4" y "c4cefcaa8e51051c2a40ba2ce8a3d27295af9467" [];
        "infer-owner@^1.0.3" = s."infer-owner@1.0.4";
        "infer-owner@^1.0.4" = s."infer-owner@1.0.4";
        "inflight@1.0.6" = f "inflight" "1.0.6" y "49bd6331d7d02d0c09bc910a1075ba8165b56df9" [
          (s."once@^1.3.0")
          (s."wrappy@1")
          ];
        "inflight@^1.0.4" = s."inflight@1.0.6";
        "inherits@2" = s."inherits@2.0.4";
        "inherits@2.0.1" = f "inherits" "2.0.1" y "b17d08d326b4423e568eff719f91b0b1cbdf69f1" [];
        "inherits@2.0.3" = f "inherits" "2.0.3" y "633c2c83e3da42a502f52466022480f4208261de" [];
        "inherits@2.0.4" = f "inherits" "2.0.4" y "0fa2c64f932917c3433a0ded55363aae37416b7c" [];
        "inherits@^2.0.0" = s."inherits@2.0.4";
        "inherits@^2.0.1" = s."inherits@2.0.4";
        "inherits@^2.0.3" = s."inherits@2.0.4";
        "inherits@^2.0.4" = s."inherits@2.0.4";
        "inherits@~2.0.1" = s."inherits@2.0.4";
        "inherits@~2.0.3" = s."inherits@2.0.4";
        "inline-style-parser@0.1.1" = f "inline-style-parser" "0.1.1" y "ec8a3b429274e9c0a1f1c4ffa9453a7fef72cea1" [];
        "internal-slot@1.0.3" = f "internal-slot" "1.0.3" y "7347e307deeea2faac2ac6205d4bc7d34967f59c" [
          (s."get-intrinsic@^1.1.0")
          (s."has@^1.0.3")
          (s."side-channel@^1.0.4")
          ];
        "internal-slot@^1.0.3" = s."internal-slot@1.0.3";
        "interpret@2.2.0" = f "interpret" "2.2.0" y "1a78a0b5965c40a5416d007ad6f50ad27c417df9" [];
        "interpret@^2.2.0" = s."interpret@2.2.0";
        "invariant@2.2.4" = f "invariant" "2.2.4" y "610f3c92c9359ce1db616e538008d23ff35158e6" [
          (s."loose-envify@^1.0.0")
          ];
        "invariant@^2.2.4" = s."invariant@2.2.4";
        "ip@1.1.5" = f "ip" "1.1.5" y "bdded70114290828c0a039e72ef25f5aaec4354a" [];
        "ip@^1.1.5" = s."ip@1.1.5";
        "ipaddr.js@1.9.1" = f "ipaddr.js" "1.9.1" y "bff38543eeb8984825079ff3a2a8e6cbd46781b3" [];
        "is-absolute-url@3.0.3" = f "is-absolute-url" "3.0.3" y "96c6a22b6a23929b11ea0afb1836c36ad4a5d698" [];
        "is-absolute-url@^3.0.0" = s."is-absolute-url@3.0.3";
        "is-accessor-descriptor@0.1.6" = f "is-accessor-descriptor" "0.1.6" y "a9e12cb3ae8d876727eeef3843f8a0897b5c98d6" [
          (s."kind-of@^3.0.2")
          ];
        "is-accessor-descriptor@1.0.0" = f "is-accessor-descriptor" "1.0.0" y "169c2f6d3df1f992618072365c9b0ea1f6878656" [
          (s."kind-of@^6.0.0")
          ];
        "is-accessor-descriptor@^0.1.6" = s."is-accessor-descriptor@0.1.6";
        "is-accessor-descriptor@^1.0.0" = s."is-accessor-descriptor@1.0.0";
        "is-alphabetical@1.0.4" = f "is-alphabetical" "1.0.4" y "9e7d6b94916be22153745d184c298cbf986a686d" [];
        "is-alphabetical@^1.0.0" = s."is-alphabetical@1.0.4";
        "is-alphanumerical@1.0.4" = f "is-alphanumerical" "1.0.4" y "7eb9a2431f855f6b1ef1a78e326df515696c4dbf" [
          (s."is-alphabetical@^1.0.0")
          (s."is-decimal@^1.0.0")
          ];
        "is-alphanumerical@^1.0.0" = s."is-alphanumerical@1.0.4";
        "is-arguments@1.1.1" = f "is-arguments" "1.1.1" y "15b3f88fda01f2a97fec84ca761a560f123efa9b" [
          (s."call-bind@^1.0.2")
          (s."has-tostringtag@^1.0.0")
          ];
        "is-arguments@^1.1.0" = s."is-arguments@1.1.1";
        "is-arrayish@0.2.1" = f "is-arrayish" "0.2.1" y "77c99840527aa8ecb1a8ba697b80645a7a926a9d" [];
        "is-arrayish@^0.2.1" = s."is-arrayish@0.2.1";
        "is-bigint@1.0.4" = f "is-bigint" "1.0.4" y "08147a1875bc2b32005d41ccd8291dffc6691df3" [
          (s."has-bigints@^1.0.1")
          ];
        "is-bigint@^1.0.1" = s."is-bigint@1.0.4";
        "is-binary-path@1.0.1" = f "is-binary-path" "1.0.1" y "75f16642b480f187a711c814161fd3a4a7655898" [
          (s."binary-extensions@^1.0.0")
          ];
        "is-binary-path@2.1.0" = f "is-binary-path" "2.1.0" y "ea1f7f3b80f064236e83470f86c09c254fb45b09" [
          (s."binary-extensions@^2.0.0")
          ];
        "is-binary-path@^1.0.0" = s."is-binary-path@1.0.1";
        "is-binary-path@~2.1.0" = s."is-binary-path@2.1.0";
        "is-boolean-object@1.1.2" = f "is-boolean-object" "1.1.2" y "5c6dc200246dd9321ae4b885a114bb1f75f63719" [
          (s."call-bind@^1.0.2")
          (s."has-tostringtag@^1.0.0")
          ];
        "is-boolean-object@^1.1.0" = s."is-boolean-object@1.1.2";
        "is-buffer@1.1.6" = f "is-buffer" "1.1.6" y "efaa2ea9daa0d7ab2ea13a97b2b8ad51fefbe8be" [];
        "is-buffer@2.0.5" = f "is-buffer" "2.0.5" y "ebc252e400d22ff8d77fa09888821a24a658c191" [];
        "is-buffer@^1.1.5" = s."is-buffer@1.1.6";
        "is-buffer@^2.0.0" = s."is-buffer@2.0.5";
        "is-callable@1.2.4" = f "is-callable" "1.2.4" y "47301d58dd0259407865547853df6d61fe471945" [];
        "is-callable@^1.1.4" = s."is-callable@1.2.4";
        "is-callable@^1.2.4" = s."is-callable@1.2.4";
        "is-ci@2.0.0" = f "is-ci" "2.0.0" y "6bc6334181810e04b5c22b3d589fdca55026404c" [
          (s."ci-info@^2.0.0")
          ];
        "is-ci@^2.0.0" = s."is-ci@2.0.0";
        "is-core-module@2.8.1" = f "is-core-module" "2.8.1" y "f59fdfca701d5879d0a6b100a40aa1560ce27211" [
          (s."has@^1.0.3")
          ];
        "is-core-module@^2.8.1" = s."is-core-module@2.8.1";
        "is-data-descriptor@0.1.4" = f "is-data-descriptor" "0.1.4" y "0b5ee648388e2c860282e793f1856fec3f301b56" [
          (s."kind-of@^3.0.2")
          ];
        "is-data-descriptor@1.0.0" = f "is-data-descriptor" "1.0.0" y "d84876321d0e7add03990406abbbbd36ba9268c7" [
          (s."kind-of@^6.0.0")
          ];
        "is-data-descriptor@^0.1.4" = s."is-data-descriptor@0.1.4";
        "is-data-descriptor@^1.0.0" = s."is-data-descriptor@1.0.0";
        "is-date-object@1.0.5" = f "is-date-object" "1.0.5" y "0841d5536e724c25597bf6ea62e1bd38298df31f" [
          (s."has-tostringtag@^1.0.0")
          ];
        "is-date-object@^1.0.1" = s."is-date-object@1.0.5";
        "is-decimal@1.0.4" = f "is-decimal" "1.0.4" y "65a3a5958a1c5b63a706e1b333d7cd9f630d3fa5" [];
        "is-decimal@^1.0.0" = s."is-decimal@1.0.4";
        "is-descriptor@0.1.6" = f "is-descriptor" "0.1.6" y "366d8240dde487ca51823b1ab9f07a10a78251ca" [
          (s."is-accessor-descriptor@^0.1.6")
          (s."is-data-descriptor@^0.1.4")
          (s."kind-of@^5.0.0")
          ];
        "is-descriptor@1.0.2" = f "is-descriptor" "1.0.2" y "3b159746a66604b04f8c81524ba365c5f14d86ec" [
          (s."is-accessor-descriptor@^1.0.0")
          (s."is-data-descriptor@^1.0.0")
          (s."kind-of@^6.0.2")
          ];
        "is-descriptor@^0.1.0" = s."is-descriptor@0.1.6";
        "is-descriptor@^1.0.0" = s."is-descriptor@1.0.2";
        "is-descriptor@^1.0.2" = s."is-descriptor@1.0.2";
        "is-docker@2.2.1" = f "is-docker" "2.2.1" y "33eeabe23cfe86f14bde4408a02c0cfb853acdaa" [];
        "is-docker@^2.0.0" = s."is-docker@2.2.1";
        "is-dom@1.1.0" = f "is-dom" "1.1.0" y "af1fced292742443bb59ca3f76ab5e80907b4e8a" [
          (s."is-object@^1.0.1")
          (s."is-window@^1.0.2")
          ];
        "is-dom@^1.0.0" = s."is-dom@1.1.0";
        "is-extendable@0.1.1" = f "is-extendable" "0.1.1" y "62b110e289a471418e3ec36a617d472e301dfc89" [];
        "is-extendable@1.0.1" = f "is-extendable" "1.0.1" y "a7470f9e426733d81bd81e1155264e3a3507cab4" [
          (s."is-plain-object@^2.0.4")
          ];
        "is-extendable@^0.1.0" = s."is-extendable@0.1.1";
        "is-extendable@^0.1.1" = s."is-extendable@0.1.1";
        "is-extendable@^1.0.1" = s."is-extendable@1.0.1";
        "is-extglob@2.1.1" = f "is-extglob" "2.1.1" y "a88c02535791f02ed37c76a1b9ea9773c833f8c2" [];
        "is-extglob@^2.1.0" = s."is-extglob@2.1.1";
        "is-extglob@^2.1.1" = s."is-extglob@2.1.1";
        "is-fullwidth-code-point@3.0.0" = f "is-fullwidth-code-point" "3.0.0" y "f116f8064fe90b3f7844a38997c0b75051269f1d" [];
        "is-fullwidth-code-point@^3.0.0" = s."is-fullwidth-code-point@3.0.0";
        "is-function@1.0.2" = f "is-function" "1.0.2" y "4f097f30abf6efadac9833b17ca5dc03f8144e08" [];
        "is-function@^1.0.2" = s."is-function@1.0.2";
        "is-glob@3.1.0" = f "is-glob" "3.1.0" y "7ba5ae24217804ac70707b96922567486cc3e84a" [
          (s."is-extglob@^2.1.0")
          ];
        "is-glob@4.0.3" = f "is-glob" "4.0.3" y "64f61e42cbbb2eec2071a9dac0b28ba1e65d5084" [
          (s."is-extglob@^2.1.1")
          ];
        "is-glob@^3.0.0" = s."is-glob@3.1.0";
        "is-glob@^3.1.0" = s."is-glob@3.1.0";
        "is-glob@^4.0.0" = s."is-glob@4.0.3";
        "is-glob@^4.0.1" = s."is-glob@4.0.3";
        "is-glob@~4.0.1" = s."is-glob@4.0.3";
        "is-hexadecimal@1.0.4" = f "is-hexadecimal" "1.0.4" y "cc35c97588da4bd49a8eedd6bc4082d44dcb23a7" [];
        "is-hexadecimal@^1.0.0" = s."is-hexadecimal@1.0.4";
        "is-map@2.0.2" = f "is-map" "2.0.2" y "00922db8c9bf73e81b7a335827bc2a43f2b91127" [];
        "is-map@^2.0.2" = s."is-map@2.0.2";
        "is-negative-zero@2.0.2" = f "is-negative-zero" "2.0.2" y "7bf6f03a28003b8b3965de3ac26f664d765f3150" [];
        "is-negative-zero@^2.0.1" = s."is-negative-zero@2.0.2";
        "is-number-object@1.0.6" = f "is-number-object" "1.0.6" y "6a7aaf838c7f0686a50b4553f7e54a96494e89f0" [
          (s."has-tostringtag@^1.0.0")
          ];
        "is-number-object@^1.0.4" = s."is-number-object@1.0.6";
        "is-number@3.0.0" = f "is-number" "3.0.0" y "24fd6201a4782cf50561c810276afc7d12d71195" [
          (s."kind-of@^3.0.2")
          ];
        "is-number@7.0.0" = f "is-number" "7.0.0" y "7535345b896734d5f80c4d06c50955527a14f12b" [];
        "is-number@^3.0.0" = s."is-number@3.0.0";
        "is-number@^7.0.0" = s."is-number@7.0.0";
        "is-object@1.0.2" = f "is-object" "1.0.2" y "a56552e1c665c9e950b4a025461da87e72f86fcf" [];
        "is-object@^1.0.1" = s."is-object@1.0.2";
        "is-plain-obj@2.1.0" = f "is-plain-obj" "2.1.0" y "45e42e37fccf1f40da8e5f76ee21515840c09287" [];
        "is-plain-obj@^2.0.0" = s."is-plain-obj@2.1.0";
        "is-plain-object@2.0.4" = f "is-plain-object" "2.0.4" y "2c163b3fafb1b606d9d17928f05c2a1c38e07677" [
          (s."isobject@^3.0.1")
          ];
        "is-plain-object@5.0.0" = f "is-plain-object" "5.0.0" y "4427f50ab3429e9025ea7d52e9043a9ef4159344" [];
        "is-plain-object@^2.0.3" = s."is-plain-object@2.0.4";
        "is-plain-object@^2.0.4" = s."is-plain-object@2.0.4";
        "is-regex@1.1.4" = f "is-regex" "1.1.4" y "eef5663cd59fa4c0ae339505323df6854bb15958" [
          (s."call-bind@^1.0.2")
          (s."has-tostringtag@^1.0.0")
          ];
        "is-regex@^1.1.2" = s."is-regex@1.1.4";
        "is-regex@^1.1.4" = s."is-regex@1.1.4";
        "is-set@2.0.2" = f "is-set" "2.0.2" y "90755fa4c2562dc1c5d4024760d6119b94ca18ec" [];
        "is-set@^2.0.2" = s."is-set@2.0.2";
        "is-shared-array-buffer@1.0.1" = f "is-shared-array-buffer" "1.0.1" y "97b0c85fbdacb59c9c446fe653b82cf2b5b7cfe6" [];
        "is-shared-array-buffer@^1.0.1" = s."is-shared-array-buffer@1.0.1";
        "is-stream@1.1.0" = f "is-stream" "1.1.0" y "12d4a3dd4e68e0b79ceb8dbc84173ae80d91ca44" [];
        "is-stream@^1.1.0" = s."is-stream@1.1.0";
        "is-string@1.0.7" = f "is-string" "1.0.7" y "0dd12bf2006f255bb58f695110eff7491eebc0fd" [
          (s."has-tostringtag@^1.0.0")
          ];
        "is-string@^1.0.5" = s."is-string@1.0.7";
        "is-string@^1.0.7" = s."is-string@1.0.7";
        "is-symbol@1.0.4" = f "is-symbol" "1.0.4" y "a6dac93b635b063ca6872236de88910a57af139c" [
          (s."has-symbols@^1.0.2")
          ];
        "is-symbol@^1.0.2" = s."is-symbol@1.0.4";
        "is-symbol@^1.0.3" = s."is-symbol@1.0.4";
        "is-typedarray@1.0.0" = f "is-typedarray" "1.0.0" y "e479c80858df0c1b11ddda6940f96011fcda4a9a" [];
        "is-typedarray@^1.0.0" = s."is-typedarray@1.0.0";
        "is-weakref@1.0.2" = f "is-weakref" "1.0.2" y "9529f383a9338205e89765e0392efc2f100f06f2" [
          (s."call-bind@^1.0.2")
          ];
        "is-weakref@^1.0.1" = s."is-weakref@1.0.2";
        "is-whitespace-character@1.0.4" = f "is-whitespace-character" "1.0.4" y "0858edd94a95594c7c9dd0b5c174ec6e45ee4aa7" [];
        "is-whitespace-character@^1.0.0" = s."is-whitespace-character@1.0.4";
        "is-window@1.0.2" = f "is-window" "1.0.2" y "2c896ca53db97de45d3c33133a65d8c9f563480d" [];
        "is-window@^1.0.2" = s."is-window@1.0.2";
        "is-windows@1.0.2" = f "is-windows" "1.0.2" y "d1850eb9791ecd18e6182ce12a30f396634bb19d" [];
        "is-windows@^1.0.2" = s."is-windows@1.0.2";
        "is-word-character@1.0.4" = f "is-word-character" "1.0.4" y "ce0e73216f98599060592f62ff31354ddbeb0230" [];
        "is-word-character@^1.0.0" = s."is-word-character@1.0.4";
        "is-wsl@1.1.0" = f "is-wsl" "1.1.0" y "1f16e4aa22b04d1336b66188a66af3c600c3a66d" [];
        "is-wsl@2.2.0" = f "is-wsl" "2.2.0" y "74a4c76e77ca9fd3f932f290c17ea326cd157271" [
          (s."is-docker@^2.0.0")
          ];
        "is-wsl@^1.1.0" = s."is-wsl@1.1.0";
        "is-wsl@^2.1.1" = s."is-wsl@2.2.0";
        "isarray@1.0.0" = f "isarray" "1.0.0" y "bb935d48582cba168c06834957a54a3e07124f11" [];
        "isarray@2.0.5" = f "isarray" "2.0.5" y "8af1e4c1221244cc62459faf38940d4e644a5723" [];
        "isarray@^1.0.0" = s."isarray@1.0.0";
        "isarray@^2.0.5" = s."isarray@2.0.5";
        "isarray@~1.0.0" = s."isarray@1.0.0";
        "isexe@2.0.0" = f "isexe" "2.0.0" y "e8fbf374dc556ff8947a10dcb0572d633f2cfa10" [];
        "isexe@^2.0.0" = s."isexe@2.0.0";
        "isobject@2.1.0" = f "isobject" "2.1.0" y "f065561096a3f1da2ef46272f815c840d87e0c89" [
          (s."isarray@1.0.0")
          ];
        "isobject@3.0.1" = f "isobject" "3.0.1" y "4e431e92b11a9731636aa1f9c8d1ccbcfdab78df" [];
        "isobject@4.0.0" = f "isobject" "4.0.0" y "3f1c9155e73b192022a80819bacd0343711697b0" [];
        "isobject@^2.0.0" = s."isobject@2.1.0";
        "isobject@^3.0.0" = s."isobject@3.0.1";
        "isobject@^3.0.1" = s."isobject@3.0.1";
        "isobject@^4.0.0" = s."isobject@4.0.0";
        "istanbul-lib-coverage@3.2.0" = f "istanbul-lib-coverage" "3.2.0" y "189e7909d0a39fa5a3dfad5b03f71947770191d3" [];
        "istanbul-lib-coverage@^3.0.0" = s."istanbul-lib-coverage@3.2.0";
        "istanbul-lib-coverage@^3.0.1" = s."istanbul-lib-coverage@3.2.0";
        "istanbul-lib-coverage@^3.2.0" = s."istanbul-lib-coverage@3.2.0";
        "istanbul-lib-instrument@5.1.0" = f "istanbul-lib-instrument" "5.1.0" y "7b49198b657b27a730b8e9cb601f1e1bff24c59a" [
          (s."@babel/core@^7.12.3")
          (s."@babel/parser@^7.14.7")
          (s."@istanbuljs/schema@^0.1.2")
          (s."istanbul-lib-coverage@^3.2.0")
          (s."semver@^6.3.0")
          ];
        "istanbul-lib-instrument@^5.0.4" = s."istanbul-lib-instrument@5.1.0";
        "istanbul-lib-report@3.0.0" = f "istanbul-lib-report" "3.0.0" y "7518fe52ea44de372f460a76b5ecda9ffb73d8a6" [
          (s."istanbul-lib-coverage@^3.0.0")
          (s."make-dir@^3.0.0")
          (s."supports-color@^7.1.0")
          ];
        "istanbul-lib-report@^3.0.0" = s."istanbul-lib-report@3.0.0";
        "istanbul-reports@3.1.3" = f "istanbul-reports" "3.1.3" y "4bcae3103b94518117930d51283690960b50d3c2" [
          (s."html-escaper@^2.0.0")
          (s."istanbul-lib-report@^3.0.0")
          ];
        "istanbul-reports@^3.0.2" = s."istanbul-reports@3.1.3";
        "iterate-iterator@1.0.2" = f "iterate-iterator" "1.0.2" y "551b804c9eaa15b847ea6a7cdc2f5bf1ec150f91" [];
        "iterate-iterator@^1.0.1" = s."iterate-iterator@1.0.2";
        "iterate-value@1.0.2" = f "iterate-value" "1.0.2" y "935115bd37d006a52046535ebc8d07e9c9337f57" [
          (s."es-get-iterator@^1.0.2")
          (s."iterate-iterator@^1.0.1")
          ];
        "iterate-value@^1.0.2" = s."iterate-value@1.0.2";
        "jest-haste-map@26.6.2" = f "jest-haste-map" "26.6.2" y "dd7e60fe7dc0e9f911a23d79c5ff7fb5c2cafeaa" [
          (s."@jest/types@^26.6.2")
          (s."@types/graceful-fs@^4.1.2")
          (s."@types/node@*")
          (s."anymatch@^3.0.3")
          (s."fb-watchman@^2.0.0")
          (s."graceful-fs@^4.2.4")
          (s."jest-regex-util@^26.0.0")
          (s."jest-serializer@^26.6.2")
          (s."jest-util@^26.6.2")
          (s."jest-worker@^26.6.2")
          (s."micromatch@^4.0.2")
          (s."sane@^4.0.3")
          (s."walker@^1.0.7")
          (s."fsevents@^2.1.2")
          ];
        "jest-haste-map@^26.6.2" = s."jest-haste-map@26.6.2";
        "jest-regex-util@26.0.0" = f "jest-regex-util" "26.0.0" y "d25e7184b36e39fd466c3bc41be0971e821fee28" [];
        "jest-regex-util@^26.0.0" = s."jest-regex-util@26.0.0";
        "jest-serializer@26.6.2" = f "jest-serializer" "26.6.2" y "d139aafd46957d3a448f3a6cdabe2919ba0742d1" [
          (s."@types/node@*")
          (s."graceful-fs@^4.2.4")
          ];
        "jest-serializer@^26.6.2" = s."jest-serializer@26.6.2";
        "jest-util@26.6.2" = f "jest-util" "26.6.2" y "907535dbe4d5a6cb4c47ac9b926f6af29576cbc1" [
          (s."@jest/types@^26.6.2")
          (s."@types/node@*")
          (s."chalk@^4.0.0")
          (s."graceful-fs@^4.2.4")
          (s."is-ci@^2.0.0")
          (s."micromatch@^4.0.2")
          ];
        "jest-util@^26.6.2" = s."jest-util@26.6.2";
        "jest-worker@26.6.2" = f "jest-worker" "26.6.2" y "7f72cbc4d643c365e27b9fd775f9d0eaa9c7a8ed" [
          (s."@types/node@*")
          (s."merge-stream@^2.0.0")
          (s."supports-color@^7.0.0")
          ];
        "jest-worker@^26.5.0" = s."jest-worker@26.6.2";
        "jest-worker@^26.6.2" = s."jest-worker@26.6.2";
        "js-string-escape@1.0.1" = f "js-string-escape" "1.0.1" y "e2625badbc0d67c7533e9edc1068c587ae4137ef" [];
        "js-string-escape@^1.0.1" = s."js-string-escape@1.0.1";
        "js-tokens@4.0.0" = f "js-tokens" "4.0.0" y "19203fb59991df98e3a287050d4647cdeaf32499" [];
        "js-tokens@^3.0.0 || ^4.0.0" = s."js-tokens@4.0.0";
        "js-tokens@^4.0.0" = s."js-tokens@4.0.0";
        "js-yaml@3.14.1" = f "js-yaml" "3.14.1" y "dae812fdb3825fa306609a8717383c50c36a0537" [
          (s."argparse@^1.0.7")
          (s."esprima@^4.0.0")
          ];
        "js-yaml@^3.13.1" = s."js-yaml@3.14.1";
        "jsesc@0.5.0" = f "jsesc" "0.5.0" y "e7dee66e35d6fc16f710fe91d5cf69f70f08911d" [];
        "jsesc@2.5.2" = f "jsesc" "2.5.2" y "80564d2e483dacf6e8ef209650a67df3f0c283a4" [];
        "jsesc@^2.5.1" = s."jsesc@2.5.2";
        "jsesc@~0.5.0" = s."jsesc@0.5.0";
        "json-parse-better-errors@1.0.2" = f "json-parse-better-errors" "1.0.2" y "bb867cfb3450e69107c131d1c514bab3dc8bcaa9" [];
        "json-parse-better-errors@^1.0.2" = s."json-parse-better-errors@1.0.2";
        "json-parse-even-better-errors@2.3.1" = f "json-parse-even-better-errors" "2.3.1" y "7c47805a94319928e05777405dc12e1f7a4ee02d" [];
        "json-parse-even-better-errors@^2.3.0" = s."json-parse-even-better-errors@2.3.1";
        "json-schema-traverse@0.4.1" = f "json-schema-traverse" "0.4.1" y "69f6a87d9513ab8bb8fe63bdb0979c448e684660" [];
        "json-schema-traverse@^0.4.1" = s."json-schema-traverse@0.4.1";
        "json5@1.0.1" = f "json5" "1.0.1" y "779fb0018604fa854eacbf6252180d83543e3dbe" [
          (s."minimist@^1.2.0")
          ];
        "json5@2.2.0" = f "json5" "2.2.0" y "2dfefe720c6ba525d9ebd909950f0515316c89a3" [
          (s."minimist@^1.2.5")
          ];
        "json5@^1.0.1" = s."json5@1.0.1";
        "json5@^2.1.2" = s."json5@2.2.0";
        "json5@^2.1.3" = s."json5@2.2.0";
        "jsonfile@2.4.0" = f "jsonfile" "2.4.0" y "3736a2b428b87bbda0cc83b53fa3d633a35c2ae8" [
          (s."graceful-fs@^4.1.6")
          ];
        "jsonfile@6.1.0" = f "jsonfile" "6.1.0" y "bc55b2634793c679ec6403094eb13698a6ec0aae" [
          (s."universalify@^2.0.0")
          (s."graceful-fs@^4.1.6")
          ];
        "jsonfile@^2.1.0" = s."jsonfile@2.4.0";
        "jsonfile@^6.0.1" = s."jsonfile@6.1.0";
        "junk@3.1.0" = f "junk" "3.1.0" y "31499098d902b7e98c5d9b9c80f43457a88abfa1" [];
        "junk@^3.1.0" = s."junk@3.1.0";
        "kind-of@3.2.2" = f "kind-of" "3.2.2" y "31ea21a734bab9bbb0f32466d893aea51e4a3c64" [
          (s."is-buffer@^1.1.5")
          ];
        "kind-of@4.0.0" = f "kind-of" "4.0.0" y "20813df3d712928b207378691a45066fae72dd57" [
          (s."is-buffer@^1.1.5")
          ];
        "kind-of@5.1.0" = f "kind-of" "5.1.0" y "729c91e2d857b7a419a1f9aa65685c4c33f5845d" [];
        "kind-of@6.0.3" = f "kind-of" "6.0.3" y "07c05034a6c349fa06e24fa35aa76db4580ce4dd" [];
        "kind-of@^3.0.2" = s."kind-of@3.2.2";
        "kind-of@^3.0.3" = s."kind-of@3.2.2";
        "kind-of@^3.2.0" = s."kind-of@3.2.2";
        "kind-of@^4.0.0" = s."kind-of@4.0.0";
        "kind-of@^5.0.0" = s."kind-of@5.1.0";
        "kind-of@^6.0.0" = s."kind-of@6.0.3";
        "kind-of@^6.0.2" = s."kind-of@6.0.3";
        "klaw@1.3.1" = f "klaw" "1.3.1" y "4088433b46b3b1ba259d78785d8e96f73ba02439" [
          (s."graceful-fs@^4.1.9")
          ];
        "klaw@^1.0.0" = s."klaw@1.3.1";
        "kleur@3.0.3" = f "kleur" "3.0.3" y "a79c9ecc86ee1ce3fa6206d1216c501f147fc07e" [];
        "kleur@^3.0.3" = s."kleur@3.0.3";
        "klona@2.0.5" = f "klona" "2.0.5" y "d166574d90076395d9963aa7a928fabb8d76afbc" [];
        "klona@^2.0.4" = s."klona@2.0.5";
        "lazy-universal-dotenv@3.0.1" = f "lazy-universal-dotenv" "3.0.1" y "a6c8938414bca426ab8c9463940da451a911db38" [
          (s."@babel/runtime@^7.5.0")
          (s."app-root-dir@^1.0.2")
          (s."core-js@^3.0.4")
          (s."dotenv@^8.0.0")
          (s."dotenv-expand@^5.1.0")
          ];
        "lazy-universal-dotenv@^3.0.1" = s."lazy-universal-dotenv@3.0.1";
        "levn@0.3.0" = f "levn" "0.3.0" y "3b09924edf9f083c0490fdd4c0bc4421e04764ee" [
          (s."prelude-ls@~1.1.2")
          (s."type-check@~0.3.2")
          ];
        "levn@~0.3.0" = s."levn@0.3.0";
        "lines-and-columns@1.2.4" = f "lines-and-columns" "1.2.4" y "eca284f75d2965079309dc0ad9255abb2ebc1632" [];
        "lines-and-columns@^1.1.6" = s."lines-and-columns@1.2.4";
        "loader-runner@2.4.0" = f "loader-runner" "2.4.0" y "ed47066bfe534d7e84c4c7b9998c2a75607d9357" [];
        "loader-runner@^2.4.0" = s."loader-runner@2.4.0";
        "loader-utils@1.4.0" = f "loader-utils" "1.4.0" y "c579b5e34cb34b1a74edc6c1fb36bfa371d5a613" [
          (s."big.js@^5.2.2")
          (s."emojis-list@^3.0.0")
          (s."json5@^1.0.1")
          ];
        "loader-utils@2.0.0" = f "loader-utils" "2.0.0" y "e4cace5b816d425a166b5f097e10cd12b36064b0" [
          (s."big.js@^5.2.2")
          (s."emojis-list@^3.0.0")
          (s."json5@^2.1.2")
          ];
        "loader-utils@2.0.2" = f "loader-utils" "2.0.2" y "d6e3b4fb81870721ae4e0868ab11dd638368c129" [
          (s."big.js@^5.2.2")
          (s."emojis-list@^3.0.0")
          (s."json5@^2.1.2")
          ];
        "loader-utils@^1.2.3" = s."loader-utils@1.4.0";
        "loader-utils@^1.4.0" = s."loader-utils@1.4.0";
        "loader-utils@^2.0.0" = s."loader-utils@2.0.2";
        "locate-path@3.0.0" = f "locate-path" "3.0.0" y "dbec3b3ab759758071b58fe59fc41871af21400e" [
          (s."p-locate@^3.0.0")
          (s."path-exists@^3.0.0")
          ];
        "locate-path@5.0.0" = f "locate-path" "5.0.0" y "1afba396afd676a6d42504d0a67a3a7eb9f62aa0" [
          (s."p-locate@^4.1.0")
          ];
        "locate-path@6.0.0" = f "locate-path" "6.0.0" y "55321eb309febbc59c4801d931a72452a681d286" [
          (s."p-locate@^5.0.0")
          ];
        "locate-path@^3.0.0" = s."locate-path@3.0.0";
        "locate-path@^5.0.0" = s."locate-path@5.0.0";
        "locate-path@^6.0.0" = s."locate-path@6.0.0";
        "lodash.debounce@4.0.8" = f "lodash.debounce" "4.0.8" y "82d79bff30a67c4005ffd5e2515300ad9ca4d7af" [];
        "lodash.debounce@^4.0.8" = s."lodash.debounce@4.0.8";
        "lodash.uniq@4.5.0" = f "lodash.uniq" "4.5.0" y "d0225373aeb652adc1bc82e4945339a842754773" [];
        "lodash@4.17.21" = f "lodash" "4.17.21" y "679591c564c3bffaae8454cf0b3df370c3d6911c" [];
        "lodash@^4.17.15" = s."lodash@4.17.21";
        "lodash@^4.17.19" = s."lodash@4.17.21";
        "lodash@^4.17.20" = s."lodash@4.17.21";
        "lodash@^4.17.21" = s."lodash@4.17.21";
        "loose-envify@1.4.0" = f "loose-envify" "1.4.0" y "71ee51fa7be4caec1a63839f7e682d8132d30caf" [
          (s."js-tokens@^3.0.0 || ^4.0.0")
          ];
        "loose-envify@^1.0.0" = s."loose-envify@1.4.0";
        "loose-envify@^1.1.0" = s."loose-envify@1.4.0";
        "loose-envify@^1.4.0" = s."loose-envify@1.4.0";
        "lower-case@2.0.2" = f "lower-case" "2.0.2" y "6fa237c63dbdc4a82ca0fd882e4722dc5e634e28" [
          (s."tslib@^2.0.3")
          ];
        "lower-case@^2.0.2" = s."lower-case@2.0.2";
        "lowlight@1.20.0" = f "lowlight" "1.20.0" y "ddb197d33462ad0d93bf19d17b6c301aa3941888" [
          (s."fault@^1.0.0")
          (s."highlight.js@~10.7.0")
          ];
        "lowlight@^1.14.0" = s."lowlight@1.20.0";
        "lru-cache@5.1.1" = f "lru-cache" "5.1.1" y "1da27e6710271947695daf6848e847f01d84b920" [
          (s."yallist@^3.0.2")
          ];
        "lru-cache@6.0.0" = f "lru-cache" "6.0.0" y "6d6fe6570ebd96aaf90fcad1dafa3b2566db3a94" [
          (s."yallist@^4.0.0")
          ];
        "lru-cache@^5.1.1" = s."lru-cache@5.1.1";
        "lru-cache@^6.0.0" = s."lru-cache@6.0.0";
        "make-dir@2.1.0" = f "make-dir" "2.1.0" y "5f0310e18b8be898cc07009295a30ae41e91e6f5" [
          (s."pify@^4.0.1")
          (s."semver@^5.6.0")
          ];
        "make-dir@3.1.0" = f "make-dir" "3.1.0" y "415e967046b3a7f1d185277d84aa58203726a13f" [
          (s."semver@^6.0.0")
          ];
        "make-dir@^2.0.0" = s."make-dir@2.1.0";
        "make-dir@^2.1.0" = s."make-dir@2.1.0";
        "make-dir@^3.0.0" = s."make-dir@3.1.0";
        "make-dir@^3.0.2" = s."make-dir@3.1.0";
        "make-dir@^3.1.0" = s."make-dir@3.1.0";
        "makeerror@1.0.12" = f "makeerror" "1.0.12" y "3e5dd2079a82e812e983cc6610c4a2cb0eaa801a" [
          (s."tmpl@1.0.5")
          ];
        "map-cache@0.2.2" = f "map-cache" "0.2.2" y "c32abd0bd6525d9b051645bb4f26ac5dc98a0dbf" [];
        "map-cache@^0.2.2" = s."map-cache@0.2.2";
        "map-or-similar@1.5.0" = f "map-or-similar" "1.5.0" y "6de2653174adfb5d9edc33c69d3e92a1b76faf08" [];
        "map-or-similar@^1.5.0" = s."map-or-similar@1.5.0";
        "map-visit@1.0.0" = f "map-visit" "1.0.0" y "ecdca8f13144e660f1b5bd41f12f3479d98dfb8f" [
          (s."object-visit@^1.0.0")
          ];
        "map-visit@^1.0.0" = s."map-visit@1.0.0";
        "markdown-escapes@1.0.4" = f "markdown-escapes" "1.0.4" y "c95415ef451499d7602b91095f3c8e8975f78535" [];
        "markdown-escapes@^1.0.0" = s."markdown-escapes@1.0.4";
        "markdown-to-jsx@7.1.6" = f "markdown-to-jsx" "7.1.6" y "421487df2a66fe4231d94db653a34da033691e62" [];
        "markdown-to-jsx@^7.1.3" = s."markdown-to-jsx@7.1.6";
        "md5.js@1.3.5" = f "md5.js" "1.3.5" y "b5d07b8e3216e3e27cd728d72f70d1e6a342005f" [
          (s."hash-base@^3.0.0")
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.1.2")
          ];
        "md5.js@^1.3.4" = s."md5.js@1.3.5";
        "mdast-squeeze-paragraphs@4.0.0" = f "mdast-squeeze-paragraphs" "4.0.0" y "7c4c114679c3bee27ef10b58e2e015be79f1ef97" [
          (s."unist-util-remove@^2.0.0")
          ];
        "mdast-squeeze-paragraphs@^4.0.0" = s."mdast-squeeze-paragraphs@4.0.0";
        "mdast-util-definitions@4.0.0" = f "mdast-util-definitions" "4.0.0" y "c5c1a84db799173b4dcf7643cda999e440c24db2" [
          (s."unist-util-visit@^2.0.0")
          ];
        "mdast-util-definitions@^4.0.0" = s."mdast-util-definitions@4.0.0";
        "mdast-util-to-hast@10.0.1" = f "mdast-util-to-hast" "10.0.1" y "0cfc82089494c52d46eb0e3edb7a4eb2aea021eb" [
          (s."@types/mdast@^3.0.0")
          (s."@types/unist@^2.0.0")
          (s."mdast-util-definitions@^4.0.0")
          (s."mdurl@^1.0.0")
          (s."unist-builder@^2.0.0")
          (s."unist-util-generated@^1.0.0")
          (s."unist-util-position@^3.0.0")
          (s."unist-util-visit@^2.0.0")
          ];
        "mdast-util-to-string@1.1.0" = f "mdast-util-to-string" "1.1.0" y "27055500103f51637bd07d01da01eb1967a43527" [];
        "mdast-util-to-string@^1.0.0" = s."mdast-util-to-string@1.1.0";
        "mdurl@1.0.1" = f "mdurl" "1.0.1" y "fe85b2ec75a59037f2adfec100fd6c601761152e" [];
        "mdurl@^1.0.0" = s."mdurl@1.0.1";
        "media-typer@0.3.0" = f "media-typer" "0.3.0" y "8710d7af0aa626f8fffa1ce00168545263255748" [];
        "memfs@3.4.1" = f "memfs" "3.4.1" y "b78092f466a0dce054d63d39275b24c71d3f1305" [
          (s."fs-monkey@1.0.3")
          ];
        "memfs@^3.1.2" = s."memfs@3.4.1";
        "memoizerific@1.11.3" = f "memoizerific" "1.11.3" y "7c87a4646444c32d75438570905f2dbd1b1a805a" [
          (s."map-or-similar@^1.5.0")
          ];
        "memoizerific@^1.11.3" = s."memoizerific@1.11.3";
        "memory-fs@0.4.1" = f "memory-fs" "0.4.1" y "3a9a20b8462523e447cfbc7e8bb80ed667bfc552" [
          (s."errno@^0.1.3")
          (s."readable-stream@^2.0.1")
          ];
        "memory-fs@0.5.0" = f "memory-fs" "0.5.0" y "324c01288b88652966d161db77838720845a8e3c" [
          (s."errno@^0.1.3")
          (s."readable-stream@^2.0.1")
          ];
        "memory-fs@^0.4.1" = s."memory-fs@0.4.1";
        "memory-fs@^0.5.0" = s."memory-fs@0.5.0";
        "merge-descriptors@1.0.1" = f "merge-descriptors" "1.0.1" y "b00aaa556dd8b44568150ec9d1b953f3f90cbb61" [];
        "merge-stream@2.0.0" = f "merge-stream" "2.0.0" y "52823629a14dd00c9770fb6ad47dc6310f2c1f60" [];
        "merge-stream@^2.0.0" = s."merge-stream@2.0.0";
        "merge2@1.4.1" = f "merge2" "1.4.1" y "4368892f885e907455a6fd7dc55c0c9d404990ae" [];
        "merge2@^1.2.3" = s."merge2@1.4.1";
        "merge2@^1.3.0" = s."merge2@1.4.1";
        "merge2@^1.4.1" = s."merge2@1.4.1";
        "methods@1.1.2" = f "methods" "1.1.2" y "5529a4d67654134edcc5266656835b0f851afcee" [];
        "methods@~1.1.2" = s."methods@1.1.2";
        "microevent.ts@0.1.1" = f "microevent.ts" "0.1.1" y "70b09b83f43df5172d0205a63025bce0f7357fa0" [];
        "microevent.ts@~0.1.1" = s."microevent.ts@0.1.1";
        "micromatch@3.1.10" = f "micromatch" "3.1.10" y "70859bc95c9840952f359a068a3fc49f9ecfac23" [
          (s."arr-diff@^4.0.0")
          (s."array-unique@^0.3.2")
          (s."braces@^2.3.1")
          (s."define-property@^2.0.2")
          (s."extend-shallow@^3.0.2")
          (s."extglob@^2.0.4")
          (s."fragment-cache@^0.2.1")
          (s."kind-of@^6.0.2")
          (s."nanomatch@^1.2.9")
          (s."object.pick@^1.3.0")
          (s."regex-not@^1.0.0")
          (s."snapdragon@^0.8.1")
          (s."to-regex@^3.0.2")
          ];
        "micromatch@4.0.4" = f "micromatch" "4.0.4" y "896d519dfe9db25fce94ceb7a500919bf881ebf9" [
          (s."braces@^3.0.1")
          (s."picomatch@^2.2.3")
          ];
        "micromatch@^3.1.10" = s."micromatch@3.1.10";
        "micromatch@^3.1.4" = s."micromatch@3.1.10";
        "micromatch@^4.0.2" = s."micromatch@4.0.4";
        "micromatch@^4.0.4" = s."micromatch@4.0.4";
        "miller-rabin@4.0.1" = f "miller-rabin" "4.0.1" y "f080351c865b0dc562a8462966daa53543c78a4d" [
          (s."bn.js@^4.0.0")
          (s."brorand@^1.0.1")
          ];
        "miller-rabin@^4.0.0" = s."miller-rabin@4.0.1";
        "mime-db@1.51.0" = f "mime-db" "1.51.0" y "d9ff62451859b18342d960850dc3cfb77e63fb0c" [];
        "mime-db@>= 1.43.0 < 2" = s."mime-db@1.51.0";
        "mime-types@2.1.34" = f "mime-types" "2.1.34" y "5a712f9ec1503511a945803640fafe09d3793c24" [
          (s."mime-db@1.51.0")
          ];
        "mime-types@^2.1.12" = s."mime-types@2.1.34";
        "mime-types@^2.1.27" = s."mime-types@2.1.34";
        "mime-types@~2.1.24" = s."mime-types@2.1.34";
        "mime@1.6.0" = f "mime" "1.6.0" y "32cd9e5c64553bd58d19a568af452acff04981b1" [];
        "mime@2.6.0" = f "mime" "2.6.0" y "a2a682a95cd4d0cb1d6257e28f83da7e35800367" [];
        "mime@^2.4.4" = s."mime@2.6.0";
        "min-document@2.19.0" = f "min-document" "2.19.0" y "7bd282e3f5842ed295bb748cdd9f1ffa2c824685" [
          (s."dom-walk@^0.1.0")
          ];
        "min-document@^2.19.0" = s."min-document@2.19.0";
        "min-indent@1.0.1" = f "min-indent" "1.0.1" y "a63f681673b30571fbe8bc25686ae746eefa9869" [];
        "min-indent@^1.0.0" = s."min-indent@1.0.1";
        "minimalistic-assert@1.0.1" = f "minimalistic-assert" "1.0.1" y "2e194de044626d4a10e7f7fbc00ce73e83e4d5c7" [];
        "minimalistic-assert@^1.0.0" = s."minimalistic-assert@1.0.1";
        "minimalistic-assert@^1.0.1" = s."minimalistic-assert@1.0.1";
        "minimalistic-crypto-utils@1.0.1" = f "minimalistic-crypto-utils" "1.0.1" y "f6c00c1c0b082246e5c4d99dfb8c7c083b2b582a" [];
        "minimalistic-crypto-utils@^1.0.1" = s."minimalistic-crypto-utils@1.0.1";
        "minimatch@3.0.4" = f "minimatch" "3.0.4" y "5166e286457f03306064be5497e8dbb0c3d32083" [
          (s."brace-expansion@^1.1.7")
          ];
        "minimatch@^3.0.2" = s."minimatch@3.0.4";
        "minimatch@^3.0.4" = s."minimatch@3.0.4";
        "minimist@1.2.5" = f "minimist" "1.2.5" y "67d66014b66a6a8aaa0c083c5fd58df4e4e97602" [];
        "minimist@^1.1.1" = s."minimist@1.2.5";
        "minimist@^1.2.0" = s."minimist@1.2.5";
        "minimist@^1.2.5" = s."minimist@1.2.5";
        "minipass-collect@1.0.2" = f "minipass-collect" "1.0.2" y "22b813bf745dc6edba2576b940022ad6edc8c617" [
          (s."minipass@^3.0.0")
          ];
        "minipass-collect@^1.0.2" = s."minipass-collect@1.0.2";
        "minipass-flush@1.0.5" = f "minipass-flush" "1.0.5" y "82e7135d7e89a50ffe64610a787953c4c4cbb373" [
          (s."minipass@^3.0.0")
          ];
        "minipass-flush@^1.0.5" = s."minipass-flush@1.0.5";
        "minipass-pipeline@1.2.4" = f "minipass-pipeline" "1.2.4" y "68472f79711c084657c067c5c6ad93cddea8214c" [
          (s."minipass@^3.0.0")
          ];
        "minipass-pipeline@^1.2.2" = s."minipass-pipeline@1.2.4";
        "minipass@3.1.6" = f "minipass" "3.1.6" y "3b8150aa688a711a1521af5e8779c1d3bb4f45ee" [
          (s."yallist@^4.0.0")
          ];
        "minipass@^3.0.0" = s."minipass@3.1.6";
        "minipass@^3.1.1" = s."minipass@3.1.6";
        "minizlib@2.1.2" = f "minizlib" "2.1.2" y "e90d3466ba209b932451508a11ce3d3632145931" [
          (s."minipass@^3.0.0")
          (s."yallist@^4.0.0")
          ];
        "minizlib@^2.1.1" = s."minizlib@2.1.2";
        "mississippi@3.0.0" = f "mississippi" "3.0.0" y "ea0a3291f97e0b5e8776b363d5f0a12d94c67022" [
          (s."concat-stream@^1.5.0")
          (s."duplexify@^3.4.2")
          (s."end-of-stream@^1.1.0")
          (s."flush-write-stream@^1.0.0")
          (s."from2@^2.1.0")
          (s."parallel-transform@^1.1.0")
          (s."pump@^3.0.0")
          (s."pumpify@^1.3.3")
          (s."stream-each@^1.1.0")
          (s."through2@^2.0.0")
          ];
        "mississippi@^3.0.0" = s."mississippi@3.0.0";
        "mixin-deep@1.3.2" = f "mixin-deep" "1.3.2" y "1120b43dc359a785dce65b55b82e257ccf479566" [
          (s."for-in@^1.0.2")
          (s."is-extendable@^1.0.1")
          ];
        "mixin-deep@^1.2.0" = s."mixin-deep@1.3.2";
        "mkdirp@0.5.5" = f "mkdirp" "0.5.5" y "d91cefd62d1436ca0f41620e251288d420099def" [
          (s."minimist@^1.2.5")
          ];
        "mkdirp@1.0.4" = f "mkdirp" "1.0.4" y "3eb5ed62622756d79a5f0e2a221dfebad75c2f7e" [];
        "mkdirp@^0.5.1" = s."mkdirp@0.5.5";
        "mkdirp@^0.5.3" = s."mkdirp@0.5.5";
        "mkdirp@^1.0.3" = s."mkdirp@1.0.4";
        "mkdirp@^1.0.4" = s."mkdirp@1.0.4";
        "move-concurrently@1.0.1" = f "move-concurrently" "1.0.1" y "be2c005fda32e0b29af1f05d7c4b33214c701f92" [
          (s."aproba@^1.1.1")
          (s."copy-concurrently@^1.0.0")
          (s."fs-write-stream-atomic@^1.0.8")
          (s."mkdirp@^0.5.1")
          (s."rimraf@^2.5.4")
          (s."run-queue@^1.0.3")
          ];
        "move-concurrently@^1.0.1" = s."move-concurrently@1.0.1";
        "ms@2.0.0" = f "ms" "2.0.0" y "5608aeadfc00be6c2901df5f9861788de0d597c8" [];
        "ms@2.1.1" = f "ms" "2.1.1" y "30a5864eb3ebb0a66f2ebe6d727af06a09d86e0a" [];
        "ms@2.1.2" = f "ms" "2.1.2" y "d09d1f357b443f493382a8eb3ccd183872ae6009" [];
        "ms@2.1.3" = f "ms" "2.1.3" y "574c8138ce1d2b5861f0b44579dbadd60c6615b2" [];
        "ms@^2.1.1" = s."ms@2.1.3";
        "nan@2.15.0" = f "nan" "2.15.0" y "3f34a473ff18e15c1b5626b62903b5ad6e665fee" [];
        "nan@^2.12.1" = s."nan@2.15.0";
        "nanoid@3.2.0" = f "nanoid" "3.2.0" y "62667522da6673971cca916a6d3eff3f415ff80c" [];
        "nanoid@^3.1.23" = s."nanoid@3.2.0";
        "nanomatch@1.2.13" = f "nanomatch" "1.2.13" y "b87a8aa4fc0de8fe6be88895b38983ff265bd119" [
          (s."arr-diff@^4.0.0")
          (s."array-unique@^0.3.2")
          (s."define-property@^2.0.2")
          (s."extend-shallow@^3.0.2")
          (s."fragment-cache@^0.2.1")
          (s."is-windows@^1.0.2")
          (s."kind-of@^6.0.2")
          (s."object.pick@^1.3.0")
          (s."regex-not@^1.0.0")
          (s."snapdragon@^0.8.1")
          (s."to-regex@^3.0.1")
          ];
        "nanomatch@^1.2.9" = s."nanomatch@1.2.13";
        "negotiator@0.6.2" = f "negotiator" "0.6.2" y "feacf7ccf525a77ae9634436a64883ffeca346fb" [];
        "neo-async@2.6.2" = f "neo-async" "2.6.2" y "b4aafb93e3aeb2d8174ca53cf163ab7d7308305f" [];
        "neo-async@^2.5.0" = s."neo-async@2.6.2";
        "neo-async@^2.6.0" = s."neo-async@2.6.2";
        "neo-async@^2.6.1" = s."neo-async@2.6.2";
        "nested-error-stacks@2.1.0" = f "nested-error-stacks" "2.1.0" y "0fbdcf3e13fe4994781280524f8b96b0cdff9c61" [];
        "nested-error-stacks@^2.0.0" = s."nested-error-stacks@2.1.0";
        "nested-error-stacks@^2.1.0" = s."nested-error-stacks@2.1.0";
        "nice-try@1.0.5" = f "nice-try" "1.0.5" y "a3378a7696ce7d223e88fc9b764bd7ef1089e366" [];
        "nice-try@^1.0.4" = s."nice-try@1.0.5";
        "no-case@3.0.4" = f "no-case" "3.0.4" y "d361fd5c9800f558551a8369fc0dcd4662b6124d" [
          (s."lower-case@^2.0.2")
          (s."tslib@^2.0.3")
          ];
        "no-case@^3.0.4" = s."no-case@3.0.4";
        "node-dir@0.1.17" = f "node-dir" "0.1.17" y "5f5665d93351335caabef8f1c554516cf5f1e4e5" [
          (s."minimatch@^3.0.2")
          ];
        "node-dir@^0.1.10" = s."node-dir@0.1.17";
        "node-fetch@2.6.7" = f "node-fetch" "2.6.7" y "24de9fba827e3b4ae44dc8b20256a379160052ad" [
          (s."whatwg-url@^5.0.0")
          ];
        "node-fetch@^2.6.1" = s."node-fetch@2.6.7";
        "node-int64@0.4.0" = f "node-int64" "0.4.0" y "87a9065cdb355d3182d8f94ce11188b825c68a3b" [];
        "node-int64@^0.4.0" = s."node-int64@0.4.0";
        "node-libs-browser@2.2.1" = f "node-libs-browser" "2.2.1" y "b64f513d18338625f90346d27b0d235e631f6425" [
          (s."assert@^1.1.1")
          (s."browserify-zlib@^0.2.0")
          (s."buffer@^4.3.0")
          (s."console-browserify@^1.1.0")
          (s."constants-browserify@^1.0.0")
          (s."crypto-browserify@^3.11.0")
          (s."domain-browser@^1.1.1")
          (s."events@^3.0.0")
          (s."https-browserify@^1.0.0")
          (s."os-browserify@^0.3.0")
          (s."path-browserify@0.0.1")
          (s."process@^0.11.10")
          (s."punycode@^1.2.4")
          (s."querystring-es3@^0.2.0")
          (s."readable-stream@^2.3.3")
          (s."stream-browserify@^2.0.1")
          (s."stream-http@^2.7.2")
          (s."string_decoder@^1.0.0")
          (s."timers-browserify@^2.0.4")
          (s."tty-browserify@0.0.0")
          (s."url@^0.11.0")
          (s."util@^0.11.0")
          (s."vm-browserify@^1.0.1")
          ];
        "node-libs-browser@^2.2.1" = s."node-libs-browser@2.2.1";
        "node-releases@2.0.1" = f "node-releases" "2.0.1" y "3d1d395f204f1f2f29a54358b9fb678765ad2fc5" [];
        "node-releases@^2.0.1" = s."node-releases@2.0.1";
        "normalize-package-data@2.5.0" = f "normalize-package-data" "2.5.0" y "e66db1838b200c1dfc233225d12cb36520e234a8" [
          (s."hosted-git-info@^2.1.4")
          (s."resolve@^1.10.0")
          (s."semver@2 || 3 || 4 || 5")
          (s."validate-npm-package-license@^3.0.1")
          ];
        "normalize-package-data@^2.5.0" = s."normalize-package-data@2.5.0";
        "normalize-path@2.1.1" = f "normalize-path" "2.1.1" y "1ab28b556e198363a8c1a6f7e6fa20137fe6aed9" [
          (s."remove-trailing-separator@^1.0.1")
          ];
        "normalize-path@3.0.0" = f "normalize-path" "3.0.0" y "0dcd69ff23a1c9b11fd0978316644a0388216a65" [];
        "normalize-path@^2.1.1" = s."normalize-path@2.1.1";
        "normalize-path@^3.0.0" = s."normalize-path@3.0.0";
        "normalize-path@~3.0.0" = s."normalize-path@3.0.0";
        "normalize-range@0.1.2" = f "normalize-range" "0.1.2" y "2d10c06bdfd312ea9777695a4d28439456b75942" [];
        "normalize-range@^0.1.2" = s."normalize-range@0.1.2";
        "npm-run-path@2.0.2" = f "npm-run-path" "2.0.2" y "35a9232dfa35d7067b4cb2ddf2357b1871536c5f" [
          (s."path-key@^2.0.0")
          ];
        "npm-run-path@^2.0.0" = s."npm-run-path@2.0.2";
        "npmlog@5.0.1" = f "npmlog" "5.0.1" y "f06678e80e29419ad67ab964e0fa69959c1eb8b0" [
          (s."are-we-there-yet@^2.0.0")
          (s."console-control-strings@^1.1.0")
          (s."gauge@^3.0.0")
          (s."set-blocking@^2.0.0")
          ];
        "npmlog@^5.0.1" = s."npmlog@5.0.1";
        "nth-check@2.0.1" = f "nth-check" "2.0.1" y "2efe162f5c3da06a28959fbd3db75dbeea9f0fc2" [
          (s."boolbase@^1.0.0")
          ];
        "nth-check@^2.0.1" = s."nth-check@2.0.1";
        "num2fraction@1.2.2" = f "num2fraction" "1.2.2" y "6f682b6a027a4e9ddfa4564cd2589d1d4e669ede" [];
        "num2fraction@^1.2.2" = s."num2fraction@1.2.2";
        "object-assign@4.1.1" = f "object-assign" "4.1.1" y "2109adc7965887cfc05cbbd442cac8bfbb360863" [];
        "object-assign@^4.1.1" = s."object-assign@4.1.1";
        "object-copy@0.1.0" = f "object-copy" "0.1.0" y "7e7d858b781bd7c991a41ba975ed3812754e998c" [
          (s."copy-descriptor@^0.1.0")
          (s."define-property@^0.2.5")
          (s."kind-of@^3.0.3")
          ];
        "object-copy@^0.1.0" = s."object-copy@0.1.0";
        "object-inspect@1.12.0" = f "object-inspect" "1.12.0" y "6e2c120e868fd1fd18cb4f18c31741d0d6e776f0" [];
        "object-inspect@^1.11.0" = s."object-inspect@1.12.0";
        "object-inspect@^1.9.0" = s."object-inspect@1.12.0";
        "object-keys@1.1.1" = f "object-keys" "1.1.1" y "1c47f272df277f3b1daf061677d9c82e2322c60e" [];
        "object-keys@^1.0.12" = s."object-keys@1.1.1";
        "object-keys@^1.1.1" = s."object-keys@1.1.1";
        "object-visit@1.0.1" = f "object-visit" "1.0.1" y "f79c4493af0c5377b59fe39d395e41042dd045bb" [
          (s."isobject@^3.0.0")
          ];
        "object-visit@^1.0.0" = s."object-visit@1.0.1";
        "object.assign@4.1.2" = f "object.assign" "4.1.2" y "0ed54a342eceb37b38ff76eb831a0e788cb63940" [
          (s."call-bind@^1.0.0")
          (s."define-properties@^1.1.3")
          (s."has-symbols@^1.0.1")
          (s."object-keys@^1.1.1")
          ];
        "object.assign@^4.1.0" = s."object.assign@4.1.2";
        "object.assign@^4.1.2" = s."object.assign@4.1.2";
        "object.entries@1.1.5" = f "object.entries" "1.1.5" y "e1acdd17c4de2cd96d5a08487cfb9db84d881861" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          ];
        "object.entries@^1.1.0" = s."object.entries@1.1.5";
        "object.fromentries@2.0.5" = f "object.fromentries" "2.0.5" y "7b37b205109c21e741e605727fe8b0ad5fa08251" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          ];
        "object.fromentries@^2.0.0 || ^1.0.0" = s."object.fromentries@2.0.5";
        "object.getownpropertydescriptors@2.1.3" = f "object.getownpropertydescriptors" "2.1.3" y "b223cf38e17fefb97a63c10c91df72ccb386df9e" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          ];
        "object.getownpropertydescriptors@^2.0.3" = s."object.getownpropertydescriptors@2.1.3";
        "object.getownpropertydescriptors@^2.1.2" = s."object.getownpropertydescriptors@2.1.3";
        "object.pick@1.3.0" = f "object.pick" "1.3.0" y "87a10ac4c1694bd2e1cbf53591a66141fb5dd747" [
          (s."isobject@^3.0.1")
          ];
        "object.pick@^1.3.0" = s."object.pick@1.3.0";
        "object.values@1.1.5" = f "object.values" "1.1.5" y "959f63e3ce9ef108720333082131e4a459b716ac" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          ];
        "object.values@^1.1.0" = s."object.values@1.1.5";
        "objectorarray@1.0.5" = f "objectorarray" "1.0.5" y "2c05248bbefabd8f43ad13b41085951aac5e68a5" [];
        "objectorarray@^1.0.5" = s."objectorarray@1.0.5";
        "on-finished@2.3.0" = f "on-finished" "2.3.0" y "20f1336481b083cd75337992a16971aa2d906947" [
          (s."ee-first@1.1.1")
          ];
        "on-finished@~2.3.0" = s."on-finished@2.3.0";
        "on-headers@1.0.2" = f "on-headers" "1.0.2" y "772b0ae6aaa525c399e489adfad90c403eb3c28f" [];
        "on-headers@~1.0.2" = s."on-headers@1.0.2";
        "once@1.4.0" = f "once" "1.4.0" y "583b1aa775961d4b113ac17d9c50baef9dd76bd1" [
          (s."wrappy@1")
          ];
        "once@^1.3.0" = s."once@1.4.0";
        "once@^1.3.1" = s."once@1.4.0";
        "once@^1.4.0" = s."once@1.4.0";
        "open@7.4.2" = f "open" "7.4.2" y "b8147e26dcf3e426316c730089fd71edd29c2321" [
          (s."is-docker@^2.0.0")
          (s."is-wsl@^2.1.1")
          ];
        "open@^7.0.3" = s."open@7.4.2";
        "optionator@0.8.3" = f "optionator" "0.8.3" y "84fa1d036fe9d3c7e21d99884b601167ec8fb495" [
          (s."deep-is@~0.1.3")
          (s."fast-levenshtein@~2.0.6")
          (s."levn@~0.3.0")
          (s."prelude-ls@~1.1.2")
          (s."type-check@~0.3.2")
          (s."word-wrap@~1.2.3")
          ];
        "optionator@^0.8.1" = s."optionator@0.8.3";
        "os-browserify@0.3.0" = f "os-browserify" "0.3.0" y "854373c7f5c2315914fc9bfc6bd8238fdda1ec27" [];
        "os-browserify@^0.3.0" = s."os-browserify@0.3.0";
        "overlayscrollbars@1.13.1" = f "overlayscrollbars" "1.13.1" y "0b840a88737f43a946b9d87875a2f9e421d0338a" [];
        "overlayscrollbars@^1.13.1" = s."overlayscrollbars@1.13.1";
        "p-all@2.1.0" = f "p-all" "2.1.0" y "91419be56b7dee8fe4c5db875d55e0da084244a0" [
          (s."p-map@^2.0.0")
          ];
        "p-all@^2.1.0" = s."p-all@2.1.0";
        "p-event@4.2.0" = f "p-event" "4.2.0" y "af4b049c8acd91ae81083ebd1e6f5cae2044c1b5" [
          (s."p-timeout@^3.1.0")
          ];
        "p-event@^4.1.0" = s."p-event@4.2.0";
        "p-filter@2.1.0" = f "p-filter" "2.1.0" y "1b1472562ae7a0f742f0f3d3d3718ea66ff9c09c" [
          (s."p-map@^2.0.0")
          ];
        "p-filter@^2.1.0" = s."p-filter@2.1.0";
        "p-finally@1.0.0" = f "p-finally" "1.0.0" y "3fbcfb15b899a44123b34b6dcc18b724336a2cae" [];
        "p-finally@^1.0.0" = s."p-finally@1.0.0";
        "p-limit@2.3.0" = f "p-limit" "2.3.0" y "3dd33c647a214fdfffd835933eb086da0dc21db1" [
          (s."p-try@^2.0.0")
          ];
        "p-limit@3.1.0" = f "p-limit" "3.1.0" y "e1daccbe78d0d1388ca18c64fea38e3e57e3706b" [
          (s."yocto-queue@^0.1.0")
          ];
        "p-limit@^2.0.0" = s."p-limit@2.3.0";
        "p-limit@^2.2.0" = s."p-limit@2.3.0";
        "p-limit@^3.0.2" = s."p-limit@3.1.0";
        "p-limit@^3.1.0" = s."p-limit@3.1.0";
        "p-locate@3.0.0" = f "p-locate" "3.0.0" y "322d69a05c0264b25997d9f40cd8a891ab0064a4" [
          (s."p-limit@^2.0.0")
          ];
        "p-locate@4.1.0" = f "p-locate" "4.1.0" y "a3428bb7088b3a60292f66919278b7c297ad4f07" [
          (s."p-limit@^2.2.0")
          ];
        "p-locate@5.0.0" = f "p-locate" "5.0.0" y "83c8315c6785005e3bd021839411c9e110e6d834" [
          (s."p-limit@^3.0.2")
          ];
        "p-locate@^3.0.0" = s."p-locate@3.0.0";
        "p-locate@^4.1.0" = s."p-locate@4.1.0";
        "p-locate@^5.0.0" = s."p-locate@5.0.0";
        "p-map@2.1.0" = f "p-map" "2.1.0" y "310928feef9c9ecc65b68b17693018a665cea175" [];
        "p-map@3.0.0" = f "p-map" "3.0.0" y "d704d9af8a2ba684e2600d9a215983d4141a979d" [
          (s."aggregate-error@^3.0.0")
          ];
        "p-map@4.0.0" = f "p-map" "4.0.0" y "bb2f95a5eda2ec168ec9274e06a747c3e2904d2b" [
          (s."aggregate-error@^3.0.0")
          ];
        "p-map@^2.0.0" = s."p-map@2.1.0";
        "p-map@^3.0.0" = s."p-map@3.0.0";
        "p-map@^4.0.0" = s."p-map@4.0.0";
        "p-timeout@3.2.0" = f "p-timeout" "3.2.0" y "c7e17abc971d2a7962ef83626b35d635acf23dfe" [
          (s."p-finally@^1.0.0")
          ];
        "p-timeout@^3.1.0" = s."p-timeout@3.2.0";
        "p-try@2.2.0" = f "p-try" "2.2.0" y "cb2868540e313d61de58fafbe35ce9004d5540e6" [];
        "p-try@^2.0.0" = s."p-try@2.2.0";
        "pako@1.0.11" = f "pako" "1.0.11" y "6c9599d340d54dfd3946380252a35705a6b992bf" [];
        "pako@~1.0.5" = s."pako@1.0.11";
        "parallel-transform@1.2.0" = f "parallel-transform" "1.2.0" y "9049ca37d6cb2182c3b1d2c720be94d14a5814fc" [
          (s."cyclist@^1.0.1")
          (s."inherits@^2.0.3")
          (s."readable-stream@^2.1.5")
          ];
        "parallel-transform@^1.1.0" = s."parallel-transform@1.2.0";
        "param-case@3.0.4" = f "param-case" "3.0.4" y "7d17fe4aa12bde34d4a77d91acfb6219caad01c5" [
          (s."dot-case@^3.0.4")
          (s."tslib@^2.0.3")
          ];
        "param-case@^3.0.3" = s."param-case@3.0.4";
        "parent-module@1.0.1" = f "parent-module" "1.0.1" y "691d2709e78c79fae3a156622452d00762caaaa2" [
          (s."callsites@^3.0.0")
          ];
        "parent-module@^1.0.0" = s."parent-module@1.0.1";
        "parse-asn1@5.1.6" = f "parse-asn1" "5.1.6" y "385080a3ec13cb62a62d39409cb3e88844cdaed4" [
          (s."asn1.js@^5.2.0")
          (s."browserify-aes@^1.0.0")
          (s."evp_bytestokey@^1.0.0")
          (s."pbkdf2@^3.0.3")
          (s."safe-buffer@^5.1.1")
          ];
        "parse-asn1@^5.0.0" = s."parse-asn1@5.1.6";
        "parse-asn1@^5.1.5" = s."parse-asn1@5.1.6";
        "parse-entities@2.0.0" = f "parse-entities" "2.0.0" y "53c6eb5b9314a1f4ec99fa0fdf7ce01ecda0cbe8" [
          (s."character-entities@^1.0.0")
          (s."character-entities-legacy@^1.0.0")
          (s."character-reference-invalid@^1.0.0")
          (s."is-alphanumerical@^1.0.0")
          (s."is-decimal@^1.0.0")
          (s."is-hexadecimal@^1.0.0")
          ];
        "parse-entities@^2.0.0" = s."parse-entities@2.0.0";
        "parse-json@5.2.0" = f "parse-json" "5.2.0" y "c76fc66dee54231c962b22bcc8a72cf2f99753cd" [
          (s."@babel/code-frame@^7.0.0")
          (s."error-ex@^1.3.1")
          (s."json-parse-even-better-errors@^2.3.0")
          (s."lines-and-columns@^1.1.6")
          ];
        "parse-json@^5.0.0" = s."parse-json@5.2.0";
        "parse5@6.0.1" = f "parse5" "6.0.1" y "e1a1c085c569b3dc08321184f19a39cc27f7c30b" [];
        "parse5@^6.0.0" = s."parse5@6.0.1";
        "parseurl@1.3.3" = f "parseurl" "1.3.3" y "9da19e7bee8d12dff0513ed5b76957793bc2e8d4" [];
        "parseurl@~1.3.2" = s."parseurl@1.3.3";
        "parseurl@~1.3.3" = s."parseurl@1.3.3";
        "pascal-case@3.1.2" = f "pascal-case" "3.1.2" y "b48e0ef2b98e205e7c1dae747d0b1508237660eb" [
          (s."no-case@^3.0.4")
          (s."tslib@^2.0.3")
          ];
        "pascal-case@^3.1.2" = s."pascal-case@3.1.2";
        "pascalcase@0.1.1" = f "pascalcase" "0.1.1" y "b363e55e8006ca6fe21784d2db22bd15d7917f14" [];
        "pascalcase@^0.1.1" = s."pascalcase@0.1.1";
        "path-browserify@0.0.1" = f "path-browserify" "0.0.1" y "e6c4ddd7ed3aa27c68a20cc4e50e1a4ee83bbc4a" [];
        "path-dirname@1.0.2" = f "path-dirname" "1.0.2" y "cc33d24d525e099a5388c0336c6e32b9160609e0" [];
        "path-dirname@^1.0.0" = s."path-dirname@1.0.2";
        "path-exists@3.0.0" = f "path-exists" "3.0.0" y "ce0ebeaa5f78cb18925ea7d810d7b59b010fd515" [];
        "path-exists@4.0.0" = f "path-exists" "4.0.0" y "513bdbe2d3b95d7762e8c1137efa195c6c61b5b3" [];
        "path-exists@^3.0.0" = s."path-exists@3.0.0";
        "path-exists@^4.0.0" = s."path-exists@4.0.0";
        "path-is-absolute@1.0.1" = f "path-is-absolute" "1.0.1" y "174b9268735534ffbc7ace6bf53a5a9e1b5c5f5f" [];
        "path-is-absolute@^1.0.0" = s."path-is-absolute@1.0.1";
        "path-key@2.0.1" = f "path-key" "2.0.1" y "411cadb574c5a140d3a4b1910d40d80cc9f40b40" [];
        "path-key@3.1.1" = f "path-key" "3.1.1" y "581f6ade658cbba65a0d3380de7753295054f375" [];
        "path-key@^2.0.0" = s."path-key@2.0.1";
        "path-key@^2.0.1" = s."path-key@2.0.1";
        "path-key@^3.1.0" = s."path-key@3.1.1";
        "path-parse@1.0.7" = f "path-parse" "1.0.7" y "fbc114b60ca42b30d9daf5858e4bd68bbedb6735" [];
        "path-parse@^1.0.7" = s."path-parse@1.0.7";
        "path-to-regexp@0.1.7" = f "path-to-regexp" "0.1.7" y "df604178005f522f15eb4490e7247a1bfaa67f8c" [];
        "path-type@3.0.0" = f "path-type" "3.0.0" y "cef31dc8e0a1a3bb0d105c0cd97cf3bf47f4e36f" [
          (s."pify@^3.0.0")
          ];
        "path-type@4.0.0" = f "path-type" "4.0.0" y "84ed01c0a7ba380afe09d90a8c180dcd9d03043b" [];
        "path-type@^3.0.0" = s."path-type@3.0.0";
        "path-type@^4.0.0" = s."path-type@4.0.0";
        "pbkdf2@3.1.2" = f "pbkdf2" "3.1.2" y "dd822aa0887580e52f1a039dc3eda108efae3075" [
          (s."create-hash@^1.1.2")
          (s."create-hmac@^1.1.4")
          (s."ripemd160@^2.0.1")
          (s."safe-buffer@^5.0.1")
          (s."sha.js@^2.4.8")
          ];
        "pbkdf2@^3.0.3" = s."pbkdf2@3.1.2";
        "picocolors@0.2.1" = f "picocolors" "0.2.1" y "570670f793646851d1ba135996962abad587859f" [];
        "picocolors@1.0.0" = f "picocolors" "1.0.0" y "cb5bdc74ff3f51892236eaf79d68bc44564ab81c" [];
        "picocolors@^0.2.1" = s."picocolors@0.2.1";
        "picocolors@^1.0.0" = s."picocolors@1.0.0";
        "picomatch@2.3.1" = f "picomatch" "2.3.1" y "3ba3833733646d9d3e4995946c1365a67fb07a42" [];
        "picomatch@^2.0.4" = s."picomatch@2.3.1";
        "picomatch@^2.2.1" = s."picomatch@2.3.1";
        "picomatch@^2.2.3" = s."picomatch@2.3.1";
        "picomatch@^2.3.0" = s."picomatch@2.3.1";
        "pify@3.0.0" = f "pify" "3.0.0" y "e5a4acd2c101fdf3d9a4d07f0dbc4db49dd28176" [];
        "pify@4.0.1" = f "pify" "4.0.1" y "4b2cd25c50d598735c50292224fd8c6df41e3231" [];
        "pify@^3.0.0" = s."pify@3.0.0";
        "pify@^4.0.1" = s."pify@4.0.1";
        "pirates@4.0.5" = f "pirates" "4.0.5" y "feec352ea5c3268fb23a37c702ab1699f35a5f3b" [];
        "pirates@^4.0.0" = s."pirates@4.0.5";
        "pirates@^4.0.1" = s."pirates@4.0.5";
        "pkg-dir@3.0.0" = f "pkg-dir" "3.0.0" y "2749020f239ed990881b1f71210d51eb6523bea3" [
          (s."find-up@^3.0.0")
          ];
        "pkg-dir@4.2.0" = f "pkg-dir" "4.2.0" y "f099133df7ede422e81d1d8448270eeb3e4261f3" [
          (s."find-up@^4.0.0")
          ];
        "pkg-dir@5.0.0" = f "pkg-dir" "5.0.0" y "a02d6aebe6ba133a928f74aec20bafdfe6b8e760" [
          (s."find-up@^5.0.0")
          ];
        "pkg-dir@^3.0.0" = s."pkg-dir@3.0.0";
        "pkg-dir@^4.1.0" = s."pkg-dir@4.2.0";
        "pkg-dir@^5.0.0" = s."pkg-dir@5.0.0";
        "pnp-webpack-plugin@1.6.4" = f "pnp-webpack-plugin" "1.6.4" y "c9711ac4dc48a685dabafc86f8b6dd9f8df84149" [
          (s."ts-pnp@^1.1.6")
          ];
        "polished@4.1.4" = f "polished" "4.1.4" y "640293ba834109614961a700fdacbb6599fb12d0" [
          (s."@babel/runtime@^7.16.7")
          ];
        "polished@^4.0.5" = s."polished@4.1.4";
        "posix-character-classes@0.1.1" = f "posix-character-classes" "0.1.1" y "01eac0fe3b5af71a2a6c02feabb8c1fef7e00eab" [];
        "posix-character-classes@^0.1.0" = s."posix-character-classes@0.1.1";
        "postcss-flexbugs-fixes@4.2.1" = f "postcss-flexbugs-fixes" "4.2.1" y "9218a65249f30897deab1033aced8578562a6690" [
          (s."postcss@^7.0.26")
          ];
        "postcss-flexbugs-fixes@^4.2.1" = s."postcss-flexbugs-fixes@4.2.1";
        "postcss-loader@4.3.0" = f "postcss-loader" "4.3.0" y "2c4de9657cd4f07af5ab42bd60a673004da1b8cc" [
          (s."cosmiconfig@^7.0.0")
          (s."klona@^2.0.4")
          (s."loader-utils@^2.0.0")
          (s."schema-utils@^3.0.0")
          (s."semver@^7.3.4")
          ];
        "postcss-loader@^4.2.0" = s."postcss-loader@4.3.0";
        "postcss-modules-extract-imports@2.0.0" = f "postcss-modules-extract-imports" "2.0.0" y "818719a1ae1da325f9832446b01136eeb493cd7e" [
          (s."postcss@^7.0.5")
          ];
        "postcss-modules-extract-imports@^2.0.0" = s."postcss-modules-extract-imports@2.0.0";
        "postcss-modules-local-by-default@3.0.3" = f "postcss-modules-local-by-default" "3.0.3" y "bb14e0cc78279d504dbdcbfd7e0ca28993ffbbb0" [
          (s."icss-utils@^4.1.1")
          (s."postcss@^7.0.32")
          (s."postcss-selector-parser@^6.0.2")
          (s."postcss-value-parser@^4.1.0")
          ];
        "postcss-modules-local-by-default@^3.0.2" = s."postcss-modules-local-by-default@3.0.3";
        "postcss-modules-scope@2.2.0" = f "postcss-modules-scope" "2.2.0" y "385cae013cc7743f5a7d7602d1073a89eaae62ee" [
          (s."postcss@^7.0.6")
          (s."postcss-selector-parser@^6.0.0")
          ];
        "postcss-modules-scope@^2.2.0" = s."postcss-modules-scope@2.2.0";
        "postcss-modules-values@3.0.0" = f "postcss-modules-values" "3.0.0" y "5b5000d6ebae29b4255301b4a3a54574423e7f10" [
          (s."icss-utils@^4.0.0")
          (s."postcss@^7.0.6")
          ];
        "postcss-modules-values@^3.0.0" = s."postcss-modules-values@3.0.0";
        "postcss-selector-parser@6.0.9" = f "postcss-selector-parser" "6.0.9" y "ee71c3b9ff63d9cd130838876c13a2ec1a992b2f" [
          (s."cssesc@^3.0.0")
          (s."util-deprecate@^1.0.2")
          ];
        "postcss-selector-parser@^6.0.0" = s."postcss-selector-parser@6.0.9";
        "postcss-selector-parser@^6.0.2" = s."postcss-selector-parser@6.0.9";
        "postcss-value-parser@4.2.0" = f "postcss-value-parser" "4.2.0" y "723c09920836ba6d3e5af019f92bc0971c02e514" [];
        "postcss-value-parser@^4.1.0" = s."postcss-value-parser@4.2.0";
        "postcss@7.0.39" = f "postcss" "7.0.39" y "9624375d965630e2e1f2c02a935c82a59cb48309" [
          (s."picocolors@^0.2.1")
          (s."source-map@^0.6.1")
          ];
        "postcss@^7.0.14" = s."postcss@7.0.39";
        "postcss@^7.0.26" = s."postcss@7.0.39";
        "postcss@^7.0.32" = s."postcss@7.0.39";
        "postcss@^7.0.36" = s."postcss@7.0.39";
        "postcss@^7.0.5" = s."postcss@7.0.39";
        "postcss@^7.0.6" = s."postcss@7.0.39";
        "prelude-ls@1.1.2" = f "prelude-ls" "1.1.2" y "21932a549f5e52ffd9a827f570e04be62a97da54" [];
        "prelude-ls@~1.1.2" = s."prelude-ls@1.1.2";
        "prettier@2.3.0" = f "prettier" "2.3.0" y "b6a5bf1284026ae640f17f7ff5658a7567fc0d18" [];
        "prettier@>=2.2.1 <=2.3.0" = s."prettier@2.3.0";
        "pretty-error@2.1.2" = f "pretty-error" "2.1.2" y "be89f82d81b1c86ec8fdfbc385045882727f93b6" [
          (s."lodash@^4.17.20")
          (s."renderkid@^2.0.4")
          ];
        "pretty-error@^2.1.1" = s."pretty-error@2.1.2";
        "pretty-hrtime@1.0.3" = f "pretty-hrtime" "1.0.3" y "b7e3ea42435a4c9b2759d99e0f201eb195802ee1" [];
        "pretty-hrtime@^1.0.3" = s."pretty-hrtime@1.0.3";
        "prismjs@1.25.0" = f "prismjs" "1.25.0" y "6f822df1bdad965734b310b315a23315cf999756" [];
        "prismjs@1.26.0" = f "prismjs" "1.26.0" y "16881b594828bb6b45296083a8cbab46b0accd47" [];
        "prismjs@^1.21.0" = s."prismjs@1.26.0";
        "prismjs@~1.25.0" = s."prismjs@1.25.0";
        "process-nextick-args@2.0.1" = f "process-nextick-args" "2.0.1" y "7820d9b16120cc55ca9ae7792680ae7dba6d7fe2" [];
        "process-nextick-args@~2.0.0" = s."process-nextick-args@2.0.1";
        "process@0.11.10" = f "process" "0.11.10" y "7332300e840161bda3e69a1d1d91a7d4bc16f182" [];
        "process@^0.11.10" = s."process@0.11.10";
        "promise-inflight@1.0.1" = f "promise-inflight" "1.0.1" y "98472870bf228132fcbdd868129bad12c3c029e3" [];
        "promise-inflight@^1.0.1" = s."promise-inflight@1.0.1";
        "promise.allsettled@1.0.5" = f "promise.allsettled" "1.0.5" y "2443f3d4b2aa8dfa560f6ac2aa6c4ea999d75f53" [
          (s."array.prototype.map@^1.0.4")
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          (s."get-intrinsic@^1.1.1")
          (s."iterate-value@^1.0.2")
          ];
        "promise.allsettled@^1.0.0" = s."promise.allsettled@1.0.5";
        "promise.prototype.finally@3.1.3" = f "promise.prototype.finally" "3.1.3" y "d3186e58fcf4df1682a150f934ccc27b7893389c" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          ];
        "promise.prototype.finally@^3.1.0" = s."promise.prototype.finally@3.1.3";
        "prompts@2.4.2" = f "prompts" "2.4.2" y "7b57e73b3a48029ad10ebd44f74b01722a4cb069" [
          (s."kleur@^3.0.3")
          (s."sisteransi@^1.0.5")
          ];
        "prompts@^2.4.0" = s."prompts@2.4.2";
        "prop-types@15.8.1" = f "prop-types" "15.8.1" y "67d87bf1a694f48435cf332c24af10214a3140b5" [
          (s."loose-envify@^1.4.0")
          (s."object-assign@^4.1.1")
          (s."react-is@^16.13.1")
          ];
        "prop-types@^15.0.0" = s."prop-types@15.8.1";
        "prop-types@^15.6.0" = s."prop-types@15.8.1";
        "prop-types@^15.7.2" = s."prop-types@15.8.1";
        "property-information@5.6.0" = f "property-information" "5.6.0" y "61675545fb23002f245c6540ec46077d4da3ed69" [
          (s."xtend@^4.0.0")
          ];
        "property-information@^5.0.0" = s."property-information@5.6.0";
        "property-information@^5.3.0" = s."property-information@5.6.0";
        "proxy-addr@2.0.7" = f "proxy-addr" "2.0.7" y "f19fe69ceab311eeb94b42e70e8c2070f9ba1025" [
          (s."forwarded@0.2.0")
          (s."ipaddr.js@1.9.1")
          ];
        "proxy-addr@~2.0.7" = s."proxy-addr@2.0.7";
        "prr@1.0.1" = f "prr" "1.0.1" y "d3fc114ba06995a45ec6893f484ceb1d78f5f476" [];
        "prr@~1.0.1" = s."prr@1.0.1";
        "public-encrypt@4.0.3" = f "public-encrypt" "4.0.3" y "4fcc9d77a07e48ba7527e7cbe0de33d0701331e0" [
          (s."bn.js@^4.1.0")
          (s."browserify-rsa@^4.0.0")
          (s."create-hash@^1.1.0")
          (s."parse-asn1@^5.0.0")
          (s."randombytes@^2.0.1")
          (s."safe-buffer@^5.1.2")
          ];
        "public-encrypt@^4.0.0" = s."public-encrypt@4.0.3";
        "pump@2.0.1" = f "pump" "2.0.1" y "12399add6e4cf7526d973cbc8b5ce2e2908b3909" [
          (s."end-of-stream@^1.1.0")
          (s."once@^1.3.1")
          ];
        "pump@3.0.0" = f "pump" "3.0.0" y "b4a2116815bde2f4e1ea602354e8c75565107a64" [
          (s."end-of-stream@^1.1.0")
          (s."once@^1.3.1")
          ];
        "pump@^2.0.0" = s."pump@2.0.1";
        "pump@^3.0.0" = s."pump@3.0.0";
        "pumpify@1.5.1" = f "pumpify" "1.5.1" y "36513be246ab27570b1a374a5ce278bfd74370ce" [
          (s."duplexify@^3.6.0")
          (s."inherits@^2.0.3")
          (s."pump@^2.0.0")
          ];
        "pumpify@^1.3.3" = s."pumpify@1.5.1";
        "punycode@1.3.2" = f "punycode" "1.3.2" y "9653a036fb7c1ee42342f2325cceefea3926c48d" [];
        "punycode@1.4.1" = f "punycode" "1.4.1" y "c0d5a63b2718800ad8e1eb0fa5269c84dd41845e" [];
        "punycode@2.1.1" = f "punycode" "2.1.1" y "b58b010ac40c22c5657616c8d2c2c02c7bf479ec" [];
        "punycode@^1.2.4" = s."punycode@1.4.1";
        "punycode@^2.1.0" = s."punycode@2.1.1";
        "qs@6.10.3" = f "qs" "6.10.3" y "d6cde1b2ffca87b5aa57889816c5f81535e22e8e" [
          (s."side-channel@^1.0.4")
          ];
        "qs@6.9.6" = f "qs" "6.9.6" y "26ed3c8243a431b2924aca84cc90471f35d5a0ee" [];
        "qs@^6.10.0" = s."qs@6.10.3";
        "querystring-es3@0.2.1" = f "querystring-es3" "0.2.1" y "9ec61f79049875707d69414596fd907a4d711e73" [];
        "querystring-es3@^0.2.0" = s."querystring-es3@0.2.1";
        "querystring@0.2.0" = f "querystring" "0.2.0" y "b209849203bb25df820da756e747005878521620" [];
        "querystring@0.2.1" = f "querystring" "0.2.1" y "40d77615bb09d16902a85c3e38aa8b5ed761c2dd" [];
        "querystring@^0.2.0" = s."querystring@0.2.1";
        "queue-microtask@1.2.3" = f "queue-microtask" "1.2.3" y "4929228bbc724dfac43e0efb058caf7b6cfb6243" [];
        "queue-microtask@^1.2.2" = s."queue-microtask@1.2.3";
        "ramda@0.21.0" = f "ramda" "0.21.0" y "a001abedb3ff61077d4ff1d577d44de77e8d0a35" [];
        "ramda@^0.21.0" = s."ramda@0.21.0";
        "randombytes@2.1.0" = f "randombytes" "2.1.0" y "df6f84372f0270dc65cdf6291349ab7a473d4f2a" [
          (s."safe-buffer@^5.1.0")
          ];
        "randombytes@^2.0.0" = s."randombytes@2.1.0";
        "randombytes@^2.0.1" = s."randombytes@2.1.0";
        "randombytes@^2.0.5" = s."randombytes@2.1.0";
        "randombytes@^2.1.0" = s."randombytes@2.1.0";
        "randomfill@1.0.4" = f "randomfill" "1.0.4" y "c92196fc86ab42be983f1bf31778224931d61458" [
          (s."randombytes@^2.0.5")
          (s."safe-buffer@^5.1.0")
          ];
        "randomfill@^1.0.3" = s."randomfill@1.0.4";
        "range-parser@1.2.1" = f "range-parser" "1.2.1" y "3cf37023d199e1c24d1a55b84800c2f3e6468031" [];
        "range-parser@^1.2.1" = s."range-parser@1.2.1";
        "range-parser@~1.2.1" = s."range-parser@1.2.1";
        "raw-body@2.4.2" = f "raw-body" "2.4.2" y "baf3e9c21eebced59dd6533ac872b71f7b61cb32" [
          (s."bytes@3.1.1")
          (s."http-errors@1.8.1")
          (s."iconv-lite@0.4.24")
          (s."unpipe@1.0.0")
          ];
        "raw-loader@4.0.2" = f "raw-loader" "4.0.2" y "1aac6b7d1ad1501e66efdac1522c73e59a584eb6" [
          (s."loader-utils@^2.0.0")
          (s."schema-utils@^3.0.0")
          ];
        "raw-loader@^4.0.2" = s."raw-loader@4.0.2";
        "react-colorful@5.5.1" = f "react-colorful" "5.5.1" y "29d9c4e496f2ca784dd2bb5053a3a4340cfaf784" [];
        "react-colorful@^5.1.2" = s."react-colorful@5.5.1";
        "react-docgen-typescript@2.2.2" = f "react-docgen-typescript" "2.2.2" y "4611055e569edc071204aadb20e1c93e1ab1659c" [];
        "react-docgen-typescript@^2.0.0" = s."react-docgen-typescript@2.2.2";
        "react-docgen@5.4.0" = f "react-docgen" "5.4.0" y "2cd7236720ec2769252ef0421f23250b39a153a1" [
          (s."@babel/core@^7.7.5")
          (s."@babel/generator@^7.12.11")
          (s."@babel/runtime@^7.7.6")
          (s."ast-types@^0.14.2")
          (s."commander@^2.19.0")
          (s."doctrine@^3.0.0")
          (s."estree-to-babel@^3.1.0")
          (s."neo-async@^2.6.1")
          (s."node-dir@^0.1.10")
          (s."strip-indent@^3.0.0")
          ];
        "react-docgen@^5.0.0" = s."react-docgen@5.4.0";
        "react-dom@17.0.2" = f "react-dom" "17.0.2" y "ecffb6845e3ad8dbfcdc498f0d0a939736502c23" [
          (s."loose-envify@^1.1.0")
          (s."object-assign@^4.1.1")
          (s."scheduler@^0.20.2")
          ];
        "react-dom@^17.0.2" = s."react-dom@17.0.2";
        "react-draggable@4.4.4" = f "react-draggable" "4.4.4" y "5b26d9996be63d32d285a426f41055de87e59b2f" [
          (s."clsx@^1.1.1")
          (s."prop-types@^15.6.0")
          ];
        "react-draggable@^4.4.3" = s."react-draggable@4.4.4";
        "react-element-to-jsx-string@14.3.4" = f "react-element-to-jsx-string" "14.3.4" y "709125bc72f06800b68f9f4db485f2c7d31218a8" [
          (s."@base2/pretty-print-object@1.0.1")
          (s."is-plain-object@5.0.0")
          (s."react-is@17.0.2")
          ];
        "react-element-to-jsx-string@^14.3.4" = s."react-element-to-jsx-string@14.3.4";
        "react-fast-compare@3.2.0" = f "react-fast-compare" "3.2.0" y "641a9da81b6a6320f270e89724fb45a0b39e43bb" [];
        "react-fast-compare@^3.0.1" = s."react-fast-compare@3.2.0";
        "react-fast-compare@^3.2.0" = s."react-fast-compare@3.2.0";
        "react-helmet-async@1.2.2" = f "react-helmet-async" "1.2.2" y "38d58d32ebffbc01ba42b5ad9142f85722492389" [
          (s."@babel/runtime@^7.12.5")
          (s."invariant@^2.2.4")
          (s."prop-types@^15.7.2")
          (s."react-fast-compare@^3.2.0")
          (s."shallowequal@^1.1.0")
          ];
        "react-helmet-async@^1.0.7" = s."react-helmet-async@1.2.2";
        "react-inspector@5.1.1" = f "react-inspector" "5.1.1" y "58476c78fde05d5055646ed8ec02030af42953c8" [
          (s."@babel/runtime@^7.0.0")
          (s."is-dom@^1.0.0")
          (s."prop-types@^15.0.0")
          ];
        "react-inspector@^5.1.0" = s."react-inspector@5.1.1";
        "react-is@16.13.1" = f "react-is" "16.13.1" y "789729a4dc36de2999dc156dd6c1d9c18cea56a4" [];
        "react-is@17.0.2" = f "react-is" "17.0.2" y "e691d4a8e9c789365655539ab372762b0efb54f0" [];
        "react-is@^16.13.1" = s."react-is@16.13.1";
        "react-is@^16.7.0" = s."react-is@16.13.1";
        "react-is@^17.0.2" = s."react-is@17.0.2";
        "react-popper-tooltip@3.1.1" = f "react-popper-tooltip" "3.1.1" y "329569eb7b287008f04fcbddb6370452ad3f9eac" [
          (s."@babel/runtime@^7.12.5")
          (s."@popperjs/core@^2.5.4")
          (s."react-popper@^2.2.4")
          ];
        "react-popper-tooltip@^3.1.1" = s."react-popper-tooltip@3.1.1";
        "react-popper@2.2.5" = f "react-popper" "2.2.5" y "1214ef3cec86330a171671a4fbcbeeb65ee58e96" [
          (s."react-fast-compare@^3.0.1")
          (s."warning@^4.0.2")
          ];
        "react-popper@^2.2.4" = s."react-popper@2.2.5";
        "react-refresh@0.11.0" = f "react-refresh" "0.11.0" y "77198b944733f0f1f1a90e791de4541f9f074046" [];
        "react-refresh@^0.11.0" = s."react-refresh@0.11.0";
        "react-router-dom@6.2.1" = f "react-router-dom" "6.2.1" y "32ec81829152fbb8a7b045bf593a22eadf019bec" [
          (s."history@^5.2.0")
          (s."react-router@6.2.1")
          ];
        "react-router-dom@^6.0.0" = s."react-router-dom@6.2.1";
        "react-router@6.2.1" = f "react-router" "6.2.1" y "be2a97a6006ce1d9123c28934e604faef51448a3" [
          (s."history@^5.2.0")
          ];
        "react-router@^6.0.0" = s."react-router@6.2.1";
        "react-sizeme@3.0.2" = f "react-sizeme" "3.0.2" y "4a2f167905ba8f8b8d932a9e35164e459f9020e4" [
          (s."element-resize-detector@^1.2.2")
          (s."invariant@^2.2.4")
          (s."shallowequal@^1.1.0")
          (s."throttle-debounce@^3.0.1")
          ];
        "react-sizeme@^3.0.1" = s."react-sizeme@3.0.2";
        "react-syntax-highlighter@13.5.3" = f "react-syntax-highlighter" "13.5.3" y "9712850f883a3e19eb858cf93fad7bb357eea9c6" [
          (s."@babel/runtime@^7.3.1")
          (s."highlight.js@^10.1.1")
          (s."lowlight@^1.14.0")
          (s."prismjs@^1.21.0")
          (s."refractor@^3.1.0")
          ];
        "react-syntax-highlighter@^13.5.3" = s."react-syntax-highlighter@13.5.3";
        "react-textarea-autosize@8.3.3" = f "react-textarea-autosize" "8.3.3" y "f70913945369da453fd554c168f6baacd1fa04d8" [
          (s."@babel/runtime@^7.10.2")
          (s."use-composed-ref@^1.0.0")
          (s."use-latest@^1.0.0")
          ];
        "react-textarea-autosize@^8.3.0" = s."react-textarea-autosize@8.3.3";
        "react@17.0.2" = f "react" "17.0.2" y "d0b5cc516d29eb3eee383f75b62864cfb6800037" [
          (s."loose-envify@^1.1.0")
          (s."object-assign@^4.1.1")
          ];
        "react@^17.0.2" = s."react@17.0.2";
        "read-pkg-up@7.0.1" = f "read-pkg-up" "7.0.1" y "f3a6135758459733ae2b95638056e1854e7ef507" [
          (s."find-up@^4.1.0")
          (s."read-pkg@^5.2.0")
          (s."type-fest@^0.8.1")
          ];
        "read-pkg-up@^7.0.1" = s."read-pkg-up@7.0.1";
        "read-pkg@5.2.0" = f "read-pkg" "5.2.0" y "7bf295438ca5a33e56cd30e053b34ee7250c93cc" [
          (s."@types/normalize-package-data@^2.4.0")
          (s."normalize-package-data@^2.5.0")
          (s."parse-json@^5.0.0")
          (s."type-fest@^0.6.0")
          ];
        "read-pkg@^5.2.0" = s."read-pkg@5.2.0";
        "readable-stream@1 || 2" = s."readable-stream@2.3.7";
        "readable-stream@2.3.7" = f "readable-stream" "2.3.7" y "1eca1cf711aef814c04f62252a36a62f6cb23b57" [
          (s."core-util-is@~1.0.0")
          (s."inherits@~2.0.3")
          (s."isarray@~1.0.0")
          (s."process-nextick-args@~2.0.0")
          (s."safe-buffer@~5.1.1")
          (s."string_decoder@~1.1.1")
          (s."util-deprecate@~1.0.1")
          ];
        "readable-stream@3.6.0" = f "readable-stream" "3.6.0" y "337bbda3adc0706bd3e024426a286d4b4b2c9198" [
          (s."inherits@^2.0.3")
          (s."string_decoder@^1.1.1")
          (s."util-deprecate@^1.0.1")
          ];
        "readable-stream@^2.0.0" = s."readable-stream@2.3.7";
        "readable-stream@^2.0.1" = s."readable-stream@2.3.7";
        "readable-stream@^2.0.2" = s."readable-stream@2.3.7";
        "readable-stream@^2.1.5" = s."readable-stream@2.3.7";
        "readable-stream@^2.2.2" = s."readable-stream@2.3.7";
        "readable-stream@^2.3.3" = s."readable-stream@2.3.7";
        "readable-stream@^2.3.6" = s."readable-stream@2.3.7";
        "readable-stream@^3.6.0" = s."readable-stream@3.6.0";
        "readable-stream@~2.3.6" = s."readable-stream@2.3.7";
        "readdirp@2.2.1" = f "readdirp" "2.2.1" y "0e87622a3325aa33e892285caf8b4e846529a525" [
          (s."graceful-fs@^4.1.11")
          (s."micromatch@^3.1.10")
          (s."readable-stream@^2.0.2")
          ];
        "readdirp@3.6.0" = f "readdirp" "3.6.0" y "74a370bd857116e245b29cc97340cd431a02a6c7" [
          (s."picomatch@^2.2.1")
          ];
        "readdirp@^2.2.1" = s."readdirp@2.2.1";
        "readdirp@~3.6.0" = s."readdirp@3.6.0";
        "refractor@3.5.0" = f "refractor" "3.5.0" y "334586f352dda4beaf354099b48c2d18e0819aec" [
          (s."hastscript@^6.0.0")
          (s."parse-entities@^2.0.0")
          (s."prismjs@~1.25.0")
          ];
        "refractor@^3.1.0" = s."refractor@3.5.0";
        "regenerate-unicode-properties@9.0.0" = f "regenerate-unicode-properties" "9.0.0" y "54d09c7115e1f53dc2314a974b32c1c344efe326" [
          (s."regenerate@^1.4.2")
          ];
        "regenerate-unicode-properties@^9.0.0" = s."regenerate-unicode-properties@9.0.0";
        "regenerate@1.4.2" = f "regenerate" "1.4.2" y "b9346d8827e8f5a32f7ba29637d398b69014848a" [];
        "regenerate@^1.4.2" = s."regenerate@1.4.2";
        "regenerator-runtime@0.13.9" = f "regenerator-runtime" "0.13.9" y "8925742a98ffd90814988d7566ad30ca3b263b52" [];
        "regenerator-runtime@^0.13.4" = s."regenerator-runtime@0.13.9";
        "regenerator-runtime@^0.13.7" = s."regenerator-runtime@0.13.9";
        "regenerator-transform@0.14.5" = f "regenerator-transform" "0.14.5" y "c98da154683671c9c4dcb16ece736517e1b7feb4" [
          (s."@babel/runtime@^7.8.4")
          ];
        "regenerator-transform@^0.14.2" = s."regenerator-transform@0.14.5";
        "regex-not@1.0.2" = f "regex-not" "1.0.2" y "1f4ece27e00b0b65e0247a6810e6a85d83a5752c" [
          (s."extend-shallow@^3.0.2")
          (s."safe-regex@^1.1.0")
          ];
        "regex-not@^1.0.0" = s."regex-not@1.0.2";
        "regex-not@^1.0.2" = s."regex-not@1.0.2";
        "regexp.prototype.flags@1.4.1" = f "regexp.prototype.flags" "1.4.1" y "b3f4c0059af9e47eca9f3f660e51d81307e72307" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          ];
        "regexp.prototype.flags@^1.3.1" = s."regexp.prototype.flags@1.4.1";
        "regexpu-core@4.8.0" = f "regexpu-core" "4.8.0" y "e5605ba361b67b1718478501327502f4479a98f0" [
          (s."regenerate@^1.4.2")
          (s."regenerate-unicode-properties@^9.0.0")
          (s."regjsgen@^0.5.2")
          (s."regjsparser@^0.7.0")
          (s."unicode-match-property-ecmascript@^2.0.0")
          (s."unicode-match-property-value-ecmascript@^2.0.0")
          ];
        "regexpu-core@^4.7.1" = s."regexpu-core@4.8.0";
        "regjsgen@0.5.2" = f "regjsgen" "0.5.2" y "92ff295fb1deecbf6ecdab2543d207e91aa33733" [];
        "regjsgen@^0.5.2" = s."regjsgen@0.5.2";
        "regjsparser@0.7.0" = f "regjsparser" "0.7.0" y "a6b667b54c885e18b52554cb4960ef71187e9968" [
          (s."jsesc@~0.5.0")
          ];
        "regjsparser@^0.7.0" = s."regjsparser@0.7.0";
        "relateurl@0.2.7" = f "relateurl" "0.2.7" y "54dbf377e51440aca90a4cd274600d3ff2d888a9" [];
        "relateurl@^0.2.7" = s."relateurl@0.2.7";
        "remark-external-links@8.0.0" = f "remark-external-links" "8.0.0" y "308de69482958b5d1cd3692bc9b725ce0240f345" [
          (s."extend@^3.0.0")
          (s."is-absolute-url@^3.0.0")
          (s."mdast-util-definitions@^4.0.0")
          (s."space-separated-tokens@^1.0.0")
          (s."unist-util-visit@^2.0.0")
          ];
        "remark-external-links@^8.0.0" = s."remark-external-links@8.0.0";
        "remark-footnotes@2.0.0" = f "remark-footnotes" "2.0.0" y "9001c4c2ffebba55695d2dd80ffb8b82f7e6303f" [];
        "remark-mdx@1.6.22" = f "remark-mdx" "1.6.22" y "06a8dab07dcfdd57f3373af7f86bd0e992108bbd" [
          (s."@babel/core@7.12.9")
          (s."@babel/helper-plugin-utils@7.10.4")
          (s."@babel/plugin-proposal-object-rest-spread@7.12.1")
          (s."@babel/plugin-syntax-jsx@7.12.1")
          (s."@mdx-js/util@1.6.22")
          (s."is-alphabetical@1.0.4")
          (s."remark-parse@8.0.3")
          (s."unified@9.2.0")
          ];
        "remark-parse@8.0.3" = f "remark-parse" "8.0.3" y "9c62aa3b35b79a486454c690472906075f40c7e1" [
          (s."ccount@^1.0.0")
          (s."collapse-white-space@^1.0.2")
          (s."is-alphabetical@^1.0.0")
          (s."is-decimal@^1.0.0")
          (s."is-whitespace-character@^1.0.0")
          (s."is-word-character@^1.0.0")
          (s."markdown-escapes@^1.0.0")
          (s."parse-entities@^2.0.0")
          (s."repeat-string@^1.5.4")
          (s."state-toggle@^1.0.0")
          (s."trim@0.0.1")
          (s."trim-trailing-lines@^1.0.0")
          (s."unherit@^1.0.4")
          (s."unist-util-remove-position@^2.0.0")
          (s."vfile-location@^3.0.0")
          (s."xtend@^4.0.1")
          ];
        "remark-slug@6.1.0" = f "remark-slug" "6.1.0" y "0503268d5f0c4ecb1f33315c00465ccdd97923ce" [
          (s."github-slugger@^1.0.0")
          (s."mdast-util-to-string@^1.0.0")
          (s."unist-util-visit@^2.0.0")
          ];
        "remark-slug@^6.0.0" = s."remark-slug@6.1.0";
        "remark-squeeze-paragraphs@4.0.0" = f "remark-squeeze-paragraphs" "4.0.0" y "76eb0e085295131c84748c8e43810159c5653ead" [
          (s."mdast-squeeze-paragraphs@^4.0.0")
          ];
        "remove-trailing-separator@1.1.0" = f "remove-trailing-separator" "1.1.0" y "c24bce2a283adad5bc3f58e0d48249b92379d8ef" [];
        "remove-trailing-separator@^1.0.1" = s."remove-trailing-separator@1.1.0";
        "renderkid@2.0.7" = f "renderkid" "2.0.7" y "464f276a6bdcee606f4a15993f9b29fc74ca8609" [
          (s."css-select@^4.1.3")
          (s."dom-converter@^0.2.0")
          (s."htmlparser2@^6.1.0")
          (s."lodash@^4.17.21")
          (s."strip-ansi@^3.0.1")
          ];
        "renderkid@^2.0.4" = s."renderkid@2.0.7";
        "repeat-element@1.1.4" = f "repeat-element" "1.1.4" y "be681520847ab58c7568ac75fbfad28ed42d39e9" [];
        "repeat-element@^1.1.2" = s."repeat-element@1.1.4";
        "repeat-string@1.6.1" = f "repeat-string" "1.6.1" y "8dcae470e1c88abc2d600fff4a776286da75e637" [];
        "repeat-string@^1.5.4" = s."repeat-string@1.6.1";
        "repeat-string@^1.6.1" = s."repeat-string@1.6.1";
        "require-directory@2.1.1" = f "require-directory" "2.1.1" y "8c64ad5fd30dab1c976e2344ffe7f792a6a6df42" [];
        "require-directory@^2.1.1" = s."require-directory@2.1.1";
        "resolve-from@4.0.0" = f "resolve-from" "4.0.0" y "4abcd852ad32dd7baabfe9b40e00a36db5f392e6" [];
        "resolve-from@5.0.0" = f "resolve-from" "5.0.0" y "c35225843df8f776df21c57557bc087e9dfdfc69" [];
        "resolve-from@^4.0.0" = s."resolve-from@4.0.0";
        "resolve-from@^5.0.0" = s."resolve-from@5.0.0";
        "resolve-url@0.2.1" = f "resolve-url" "0.2.1" y "2c637fe77c893afd2a663fe21aa9080068e2052a" [];
        "resolve-url@^0.2.1" = s."resolve-url@0.2.1";
        "resolve@1.22.0" = f "resolve" "1.22.0" y "5e0b8c67c15df57a89bdbabe603a002f21731198" [
          (s."is-core-module@^2.8.1")
          (s."path-parse@^1.0.7")
          (s."supports-preserve-symlinks-flag@^1.0.0")
          ];
        "resolve@^1.10.0" = s."resolve@1.22.0";
        "resolve@^1.12.0" = s."resolve@1.22.0";
        "resolve@^1.14.2" = s."resolve@1.22.0";
        "resolve@^1.19.0" = s."resolve@1.22.0";
        "resolve@^1.3.2" = s."resolve@1.22.0";
        "ret@0.1.15" = f "ret" "0.1.15" y "b8a4825d5bdb1fc3f6f53c2bc33f81388681c7bc" [];
        "ret@~0.1.10" = s."ret@0.1.15";
        "reusify@1.0.4" = f "reusify" "1.0.4" y "90da382b1e126efc02146e90845a88db12925d76" [];
        "reusify@^1.0.4" = s."reusify@1.0.4";
        "rimraf@2.7.1" = f "rimraf" "2.7.1" y "35797f13a7fdadc566142c29d4f07ccad483e3ec" [
          (s."glob@^7.1.3")
          ];
        "rimraf@3.0.2" = f "rimraf" "3.0.2" y "f1a5402ba6220ad52cc1282bac1ae3aa49fd061a" [
          (s."glob@^7.1.3")
          ];
        "rimraf@^2.2.8" = s."rimraf@2.7.1";
        "rimraf@^2.5.4" = s."rimraf@2.7.1";
        "rimraf@^2.6.3" = s."rimraf@2.7.1";
        "rimraf@^3.0.0" = s."rimraf@3.0.2";
        "rimraf@^3.0.2" = s."rimraf@3.0.2";
        "ripemd160@2.0.2" = f "ripemd160" "2.0.2" y "a1c1a6f624751577ba5d07914cbc92850585890c" [
          (s."hash-base@^3.0.0")
          (s."inherits@^2.0.1")
          ];
        "ripemd160@^2.0.0" = s."ripemd160@2.0.2";
        "ripemd160@^2.0.1" = s."ripemd160@2.0.2";
        "rsvp@4.8.5" = f "rsvp" "4.8.5" y "c8f155311d167f68f21e168df71ec5b083113734" [];
        "rsvp@^4.8.4" = s."rsvp@4.8.5";
        "run-parallel@1.2.0" = f "run-parallel" "1.2.0" y "66d1368da7bdf921eb9d95bd1a9229e7f21a43ee" [
          (s."queue-microtask@^1.2.2")
          ];
        "run-parallel@^1.1.9" = s."run-parallel@1.2.0";
        "run-queue@1.0.3" = f "run-queue" "1.0.3" y "e848396f057d223f24386924618e25694161ec47" [
          (s."aproba@^1.1.1")
          ];
        "run-queue@^1.0.0" = s."run-queue@1.0.3";
        "run-queue@^1.0.3" = s."run-queue@1.0.3";
        "safe-buffer@5.1.1" = f "safe-buffer" "5.1.1" y "893312af69b2123def71f57889001671eeb2c853" [];
        "safe-buffer@5.1.2" = f "safe-buffer" "5.1.2" y "991ec69d296e0313747d59bdfd2b745c35f8828d" [];
        "safe-buffer@5.2.1" = f "safe-buffer" "5.2.1" y "1eaf9fa9bdb1fdd4ec75f58f9cdb4e6b7827eec6" [];
        "safe-buffer@^5.0.1" = s."safe-buffer@5.2.1";
        "safe-buffer@^5.1.0" = s."safe-buffer@5.2.1";
        "safe-buffer@^5.1.1" = s."safe-buffer@5.2.1";
        "safe-buffer@^5.1.2" = s."safe-buffer@5.2.1";
        "safe-buffer@^5.2.0" = s."safe-buffer@5.2.1";
        "safe-buffer@~5.1.0" = s."safe-buffer@5.1.2";
        "safe-buffer@~5.1.1" = s."safe-buffer@5.1.2";
        "safe-buffer@~5.2.0" = s."safe-buffer@5.2.1";
        "safe-regex@1.1.0" = f "safe-regex" "1.1.0" y "40a3669f3b077d1e943d44629e157dd48023bf2e" [
          (s."ret@~0.1.10")
          ];
        "safe-regex@^1.1.0" = s."safe-regex@1.1.0";
        "safer-buffer@2.1.2" = f "safer-buffer" "2.1.2" y "44fa161b0187b9549dd84bb91802f9bd8385cd6a" [];
        "safer-buffer@>= 2.1.2 < 3" = s."safer-buffer@2.1.2";
        "safer-buffer@^2.1.0" = s."safer-buffer@2.1.2";
        "sane@4.1.0" = f "sane" "4.1.0" y "ed881fd922733a6c461bc189dc2b6c006f3ffded" [
          (s."@cnakazawa/watch@^1.0.3")
          (s."anymatch@^2.0.0")
          (s."capture-exit@^2.0.0")
          (s."exec-sh@^0.3.2")
          (s."execa@^1.0.0")
          (s."fb-watchman@^2.0.0")
          (s."micromatch@^3.1.4")
          (s."minimist@^1.1.1")
          (s."walker@~1.0.5")
          ];
        "sane@^4.0.3" = s."sane@4.1.0";
        "scheduler@0.20.2" = f "scheduler" "0.20.2" y "4baee39436e34aa93b4874bddcbf0fe8b8b50e91" [
          (s."loose-envify@^1.1.0")
          (s."object-assign@^4.1.1")
          ];
        "scheduler@^0.20.2" = s."scheduler@0.20.2";
        "schema-utils@1.0.0" = f "schema-utils" "1.0.0" y "0b79a93204d7b600d4b2850d1f66c2a34951c770" [
          (s."ajv@^6.1.0")
          (s."ajv-errors@^1.0.0")
          (s."ajv-keywords@^3.1.0")
          ];
        "schema-utils@2.7.0" = f "schema-utils" "2.7.0" y "17151f76d8eae67fbbf77960c33c676ad9f4efc7" [
          (s."@types/json-schema@^7.0.4")
          (s."ajv@^6.12.2")
          (s."ajv-keywords@^3.4.1")
          ];
        "schema-utils@2.7.1" = f "schema-utils" "2.7.1" y "1ca4f32d1b24c590c203b8e7a50bf0ea4cd394d7" [
          (s."@types/json-schema@^7.0.5")
          (s."ajv@^6.12.4")
          (s."ajv-keywords@^3.5.2")
          ];
        "schema-utils@3.1.1" = f "schema-utils" "3.1.1" y "bc74c4b6b6995c1d88f76a8b77bea7219e0c8281" [
          (s."@types/json-schema@^7.0.8")
          (s."ajv@^6.12.5")
          (s."ajv-keywords@^3.5.2")
          ];
        "schema-utils@^1.0.0" = s."schema-utils@1.0.0";
        "schema-utils@^2.6.5" = s."schema-utils@2.7.1";
        "schema-utils@^2.7.0" = s."schema-utils@2.7.1";
        "schema-utils@^3.0.0" = s."schema-utils@3.1.1";
        "semver@2 || 3 || 4 || 5" = s."semver@5.7.1";
        "semver@5.7.1" = f "semver" "5.7.1" y "a954f931aeba508d307bbf069eff0c01c96116f7" [];
        "semver@6.3.0" = f "semver" "6.3.0" y "ee0a64c8af5e8ceea67687b133761e1becbd1d3d" [];
        "semver@7.0.0" = f "semver" "7.0.0" y "5f3ca35761e47e05b206c6daff2cf814f0316b8e" [];
        "semver@7.3.5" = f "semver" "7.3.5" y "0b621c879348d8998e4b0e4be94b3f12e6018ef7" [
          (s."lru-cache@^6.0.0")
          ];
        "semver@^5.4.1" = s."semver@5.7.1";
        "semver@^5.5.0" = s."semver@5.7.1";
        "semver@^5.6.0" = s."semver@5.7.1";
        "semver@^6.0.0" = s."semver@6.3.0";
        "semver@^6.1.1" = s."semver@6.3.0";
        "semver@^6.1.2" = s."semver@6.3.0";
        "semver@^6.3.0" = s."semver@6.3.0";
        "semver@^7.3.2" = s."semver@7.3.5";
        "semver@^7.3.4" = s."semver@7.3.5";
        "semver@^7.3.5" = s."semver@7.3.5";
        "send@0.17.2" = f "send" "0.17.2" y "926622f76601c41808012c8bf1688fe3906f7820" [
          (s."debug@2.6.9")
          (s."depd@~1.1.2")
          (s."destroy@~1.0.4")
          (s."encodeurl@~1.0.2")
          (s."escape-html@~1.0.3")
          (s."etag@~1.8.1")
          (s."fresh@0.5.2")
          (s."http-errors@1.8.1")
          (s."mime@1.6.0")
          (s."ms@2.1.3")
          (s."on-finished@~2.3.0")
          (s."range-parser@~1.2.1")
          (s."statuses@~1.5.0")
          ];
        "serialize-javascript@4.0.0" = f "serialize-javascript" "4.0.0" y "b525e1238489a5ecfc42afacc3fe99e666f4b1aa" [
          (s."randombytes@^2.1.0")
          ];
        "serialize-javascript@5.0.1" = f "serialize-javascript" "5.0.1" y "7886ec848049a462467a97d3d918ebb2aaf934f4" [
          (s."randombytes@^2.1.0")
          ];
        "serialize-javascript@^4.0.0" = s."serialize-javascript@4.0.0";
        "serialize-javascript@^5.0.1" = s."serialize-javascript@5.0.1";
        "serve-favicon@2.5.0" = f "serve-favicon" "2.5.0" y "935d240cdfe0f5805307fdfe967d88942a2cbcf0" [
          (s."etag@~1.8.1")
          (s."fresh@0.5.2")
          (s."ms@2.1.1")
          (s."parseurl@~1.3.2")
          (s."safe-buffer@5.1.1")
          ];
        "serve-favicon@^2.5.0" = s."serve-favicon@2.5.0";
        "serve-static@1.14.2" = f "serve-static" "1.14.2" y "722d6294b1d62626d41b43a013ece4598d292bfa" [
          (s."encodeurl@~1.0.2")
          (s."escape-html@~1.0.3")
          (s."parseurl@~1.3.3")
          (s."send@0.17.2")
          ];
        "set-blocking@2.0.0" = f "set-blocking" "2.0.0" y "045f9782d011ae9a6803ddd382b24392b3d890f7" [];
        "set-blocking@^2.0.0" = s."set-blocking@2.0.0";
        "set-value@2.0.1" = f "set-value" "2.0.1" y "a18d40530e6f07de4228c7defe4227af8cad005b" [
          (s."extend-shallow@^2.0.1")
          (s."is-extendable@^0.1.1")
          (s."is-plain-object@^2.0.3")
          (s."split-string@^3.0.1")
          ];
        "set-value@^2.0.0" = s."set-value@2.0.1";
        "set-value@^2.0.1" = s."set-value@2.0.1";
        "setimmediate@1.0.5" = f "setimmediate" "1.0.5" y "290cbb232e306942d7d7ea9b83732ab7856f8285" [];
        "setimmediate@^1.0.4" = s."setimmediate@1.0.5";
        "setprototypeof@1.2.0" = f "setprototypeof" "1.2.0" y "66c9a24a73f9fc28cbe66b09fed3d33dcaf1b424" [];
        "sha.js@2.4.11" = f "sha.js" "2.4.11" y "37a5cf0b81ecbc6943de109ba2960d1b26584ae7" [
          (s."inherits@^2.0.1")
          (s."safe-buffer@^5.0.1")
          ];
        "sha.js@^2.4.0" = s."sha.js@2.4.11";
        "sha.js@^2.4.8" = s."sha.js@2.4.11";
        "shallow-clone@3.0.1" = f "shallow-clone" "3.0.1" y "8f2981ad92531f55035b01fb230769a40e02efa3" [
          (s."kind-of@^6.0.2")
          ];
        "shallow-clone@^3.0.0" = s."shallow-clone@3.0.1";
        "shallowequal@1.1.0" = f "shallowequal" "1.1.0" y "188d521de95b9087404fd4dcb68b13df0ae4e7f8" [];
        "shallowequal@^1.1.0" = s."shallowequal@1.1.0";
        "shebang-command@1.2.0" = f "shebang-command" "1.2.0" y "44aac65b695b03398968c39f363fee5deafdf1ea" [
          (s."shebang-regex@^1.0.0")
          ];
        "shebang-command@2.0.0" = f "shebang-command" "2.0.0" y "ccd0af4f8835fbdc265b82461aaf0c36663f34ea" [
          (s."shebang-regex@^3.0.0")
          ];
        "shebang-command@^1.2.0" = s."shebang-command@1.2.0";
        "shebang-command@^2.0.0" = s."shebang-command@2.0.0";
        "shebang-regex@1.0.0" = f "shebang-regex" "1.0.0" y "da42f49740c0b42db2ca9728571cb190c98efea3" [];
        "shebang-regex@3.0.0" = f "shebang-regex" "3.0.0" y "ae16f1644d873ecad843b0307b143362d4c42172" [];
        "shebang-regex@^1.0.0" = s."shebang-regex@1.0.0";
        "shebang-regex@^3.0.0" = s."shebang-regex@3.0.0";
        "side-channel@1.0.4" = f "side-channel" "1.0.4" y "efce5c8fdc104ee751b25c58d4290011fa5ea2cf" [
          (s."call-bind@^1.0.0")
          (s."get-intrinsic@^1.0.2")
          (s."object-inspect@^1.9.0")
          ];
        "side-channel@^1.0.4" = s."side-channel@1.0.4";
        "signal-exit@3.0.6" = f "signal-exit" "3.0.6" y "24e630c4b0f03fea446a2bd299e62b4a6ca8d0af" [];
        "signal-exit@^3.0.0" = s."signal-exit@3.0.6";
        "signal-exit@^3.0.2" = s."signal-exit@3.0.6";
        "sisteransi@1.0.5" = f "sisteransi" "1.0.5" y "134d681297756437cc05ca01370d3a7a571075ed" [];
        "sisteransi@^1.0.5" = s."sisteransi@1.0.5";
        "slash@2.0.0" = f "slash" "2.0.0" y "de552851a1759df3a8f206535442f5ec4ddeab44" [];
        "slash@3.0.0" = f "slash" "3.0.0" y "6539be870c165adbd5240220dbe361f1bc4d4634" [];
        "slash@^2.0.0" = s."slash@2.0.0";
        "slash@^3.0.0" = s."slash@3.0.0";
        "snapdragon-node@2.1.1" = f "snapdragon-node" "2.1.1" y "6c175f86ff14bdb0724563e8f3c1b021a286853b" [
          (s."define-property@^1.0.0")
          (s."isobject@^3.0.0")
          (s."snapdragon-util@^3.0.1")
          ];
        "snapdragon-node@^2.0.1" = s."snapdragon-node@2.1.1";
        "snapdragon-util@3.0.1" = f "snapdragon-util" "3.0.1" y "f956479486f2acd79700693f6f7b805e45ab56e2" [
          (s."kind-of@^3.2.0")
          ];
        "snapdragon-util@^3.0.1" = s."snapdragon-util@3.0.1";
        "snapdragon@0.8.2" = f "snapdragon" "0.8.2" y "64922e7c565b0e14204ba1aa7d6964278d25182d" [
          (s."base@^0.11.1")
          (s."debug@^2.2.0")
          (s."define-property@^0.2.5")
          (s."extend-shallow@^2.0.1")
          (s."map-cache@^0.2.2")
          (s."source-map@^0.5.6")
          (s."source-map-resolve@^0.5.0")
          (s."use@^3.1.0")
          ];
        "snapdragon@^0.8.1" = s."snapdragon@0.8.2";
        "source-list-map@2.0.1" = f "source-list-map" "2.0.1" y "3993bd873bfc48479cca9ea3a547835c7c154b34" [];
        "source-list-map@^2.0.0" = s."source-list-map@2.0.1";
        "source-map-resolve@0.5.3" = f "source-map-resolve" "0.5.3" y "190866bece7553e1f8f267a2ee82c606b5509a1a" [
          (s."atob@^2.1.2")
          (s."decode-uri-component@^0.2.0")
          (s."resolve-url@^0.2.1")
          (s."source-map-url@^0.4.0")
          (s."urix@^0.1.0")
          ];
        "source-map-resolve@^0.5.0" = s."source-map-resolve@0.5.3";
        "source-map-support@0.5.21" = f "source-map-support" "0.5.21" y "04fe7c7f9e1ed2d662233c28cb2b35b9f63f6e4f" [
          (s."buffer-from@^1.0.0")
          (s."source-map@^0.6.0")
          ];
        "source-map-support@^0.5.16" = s."source-map-support@0.5.21";
        "source-map-support@~0.5.12" = s."source-map-support@0.5.21";
        "source-map-support@~0.5.20" = s."source-map-support@0.5.21";
        "source-map-url@0.4.1" = f "source-map-url" "0.4.1" y "0af66605a745a5a2f91cf1bbf8a7afbc283dec56" [];
        "source-map-url@^0.4.0" = s."source-map-url@0.4.1";
        "source-map@0.5.7" = f "source-map" "0.5.7" y "8a039d2d1021d22d1ea14c80d8ea468ba2ef3fcc" [];
        "source-map@0.6.1" = f "source-map" "0.6.1" y "74722af32e9614e9c287a8d0bbde48b5e2f1a263" [];
        "source-map@0.7.3" = f "source-map" "0.7.3" y "5302f8169031735226544092e64981f751750383" [];
        "source-map@^0.5.0" = s."source-map@0.5.7";
        "source-map@^0.5.6" = s."source-map@0.5.7";
        "source-map@^0.5.7" = s."source-map@0.5.7";
        "source-map@^0.6.0" = s."source-map@0.6.1";
        "source-map@^0.6.1" = s."source-map@0.6.1";
        "source-map@^0.7.3" = s."source-map@0.7.3";
        "source-map@~0.6.0" = s."source-map@0.6.1";
        "source-map@~0.6.1" = s."source-map@0.6.1";
        "source-map@~0.7.2" = s."source-map@0.7.3";
        "space-separated-tokens@1.1.5" = f "space-separated-tokens" "1.1.5" y "85f32c3d10d9682007e917414ddc5c26d1aa6899" [];
        "space-separated-tokens@^1.0.0" = s."space-separated-tokens@1.1.5";
        "spdx-correct@3.1.1" = f "spdx-correct" "3.1.1" y "dece81ac9c1e6713e5f7d1b6f17d468fa53d89a9" [
          (s."spdx-expression-parse@^3.0.0")
          (s."spdx-license-ids@^3.0.0")
          ];
        "spdx-correct@^3.0.0" = s."spdx-correct@3.1.1";
        "spdx-exceptions@2.3.0" = f "spdx-exceptions" "2.3.0" y "3f28ce1a77a00372683eade4a433183527a2163d" [];
        "spdx-exceptions@^2.1.0" = s."spdx-exceptions@2.3.0";
        "spdx-expression-parse@3.0.1" = f "spdx-expression-parse" "3.0.1" y "cf70f50482eefdc98e3ce0a6833e4a53ceeba679" [
          (s."spdx-exceptions@^2.1.0")
          (s."spdx-license-ids@^3.0.0")
          ];
        "spdx-expression-parse@^3.0.0" = s."spdx-expression-parse@3.0.1";
        "spdx-license-ids@3.0.11" = f "spdx-license-ids" "3.0.11" y "50c0d8c40a14ec1bf449bae69a0ea4685a9d9f95" [];
        "spdx-license-ids@^3.0.0" = s."spdx-license-ids@3.0.11";
        "split-string@3.1.0" = f "split-string" "3.1.0" y "7cb09dda3a86585705c64b39a6466038682e8fe2" [
          (s."extend-shallow@^3.0.0")
          ];
        "split-string@^3.0.1" = s."split-string@3.1.0";
        "split-string@^3.0.2" = s."split-string@3.1.0";
        "sprintf-js@1.0.3" = f "sprintf-js" "1.0.3" y "04e6926f662895354f3dd015203633b857297e2c" [];
        "sprintf-js@~1.0.2" = s."sprintf-js@1.0.3";
        "ssri@6.0.2" = f "ssri" "6.0.2" y "157939134f20464e7301ddba3e90ffa8f7728ac5" [
          (s."figgy-pudding@^3.5.1")
          ];
        "ssri@8.0.1" = f "ssri" "8.0.1" y "638e4e439e2ffbd2cd289776d5ca457c4f51a2af" [
          (s."minipass@^3.1.1")
          ];
        "ssri@^6.0.1" = s."ssri@6.0.2";
        "ssri@^8.0.1" = s."ssri@8.0.1";
        "stable@0.1.8" = f "stable" "0.1.8" y "836eb3c8382fe2936feaf544631017ce7d47a3cf" [];
        "stable@^0.1.8" = s."stable@0.1.8";
        "stackframe@1.2.0" = f "stackframe" "1.2.0" y "52429492d63c62eb989804c11552e3d22e779303" [];
        "stackframe@^1.1.1" = s."stackframe@1.2.0";
        "state-toggle@1.0.3" = f "state-toggle" "1.0.3" y "e123b16a88e143139b09c6852221bc9815917dfe" [];
        "state-toggle@^1.0.0" = s."state-toggle@1.0.3";
        "static-extend@0.1.2" = f "static-extend" "0.1.2" y "60809c39cbff55337226fd5e0b520f341f1fb5c6" [
          (s."define-property@^0.2.5")
          (s."object-copy@^0.1.0")
          ];
        "static-extend@^0.1.1" = s."static-extend@0.1.2";
        "statuses@1.5.0" = f "statuses" "1.5.0" y "161c7dac177659fd9811f43771fa99381478628c" [];
        "statuses@>= 1.5.0 < 2" = s."statuses@1.5.0";
        "statuses@~1.5.0" = s."statuses@1.5.0";
        "store2@2.13.1" = f "store2" "2.13.1" y "fae7b5bb9d35fc53dc61cd262df3abb2f6e59022" [];
        "store2@^2.12.0" = s."store2@2.13.1";
        "stream-browserify@2.0.2" = f "stream-browserify" "2.0.2" y "87521d38a44aa7ee91ce1cd2a47df0cb49dd660b" [
          (s."inherits@~2.0.1")
          (s."readable-stream@^2.0.2")
          ];
        "stream-browserify@^2.0.1" = s."stream-browserify@2.0.2";
        "stream-each@1.2.3" = f "stream-each" "1.2.3" y "ebe27a0c389b04fbcc233642952e10731afa9bae" [
          (s."end-of-stream@^1.1.0")
          (s."stream-shift@^1.0.0")
          ];
        "stream-each@^1.1.0" = s."stream-each@1.2.3";
        "stream-http@2.8.3" = f "stream-http" "2.8.3" y "b2d242469288a5a27ec4fe8933acf623de6514fc" [
          (s."builtin-status-codes@^3.0.0")
          (s."inherits@^2.0.1")
          (s."readable-stream@^2.3.6")
          (s."to-arraybuffer@^1.0.0")
          (s."xtend@^4.0.0")
          ];
        "stream-http@^2.7.2" = s."stream-http@2.8.3";
        "stream-shift@1.0.1" = f "stream-shift" "1.0.1" y "d7088281559ab2778424279b0877da3c392d5a3d" [];
        "stream-shift@^1.0.0" = s."stream-shift@1.0.1";
        "string-width@4.2.3" = f "string-width" "4.2.3" y "269c7117d27b05ad2e536830a8ec895ef9c6d010" [
          (s."emoji-regex@^8.0.0")
          (s."is-fullwidth-code-point@^3.0.0")
          (s."strip-ansi@^6.0.1")
          ];
        "string-width@^1.0.2 || 2 || 3 || 4" = s."string-width@4.2.3";
        "string-width@^4.0.0" = s."string-width@4.2.3";
        "string-width@^4.1.0" = s."string-width@4.2.3";
        "string-width@^4.2.0" = s."string-width@4.2.3";
        "string-width@^4.2.2" = s."string-width@4.2.3";
        "string-width@^4.2.3" = s."string-width@4.2.3";
        "string.prototype.matchall@4.0.6" = f "string.prototype.matchall" "4.0.6" y "5abb5dabc94c7b0ea2380f65ba610b3a544b15fa" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          (s."get-intrinsic@^1.1.1")
          (s."has-symbols@^1.0.2")
          (s."internal-slot@^1.0.3")
          (s."regexp.prototype.flags@^1.3.1")
          (s."side-channel@^1.0.4")
          ];
        "string.prototype.matchall@^4.0.0 || ^3.0.1" = s."string.prototype.matchall@4.0.6";
        "string.prototype.padend@3.1.3" = f "string.prototype.padend" "3.1.3" y "997a6de12c92c7cb34dc8a201a6c53d9bd88a5f1" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          ];
        "string.prototype.padend@^3.0.0" = s."string.prototype.padend@3.1.3";
        "string.prototype.padstart@3.1.3" = f "string.prototype.padstart" "3.1.3" y "4551d0117d9501692ec6000b15056ac3f816cfa5" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          (s."es-abstract@^1.19.1")
          ];
        "string.prototype.padstart@^3.0.0" = s."string.prototype.padstart@3.1.3";
        "string.prototype.trimend@1.0.4" = f "string.prototype.trimend" "1.0.4" y "e75ae90c2942c63504686c18b287b4a0b1a45f80" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          ];
        "string.prototype.trimend@^1.0.4" = s."string.prototype.trimend@1.0.4";
        "string.prototype.trimstart@1.0.4" = f "string.prototype.trimstart" "1.0.4" y "b36399af4ab2999b4c9c648bd7a3fb2bb26feeed" [
          (s."call-bind@^1.0.2")
          (s."define-properties@^1.1.3")
          ];
        "string.prototype.trimstart@^1.0.4" = s."string.prototype.trimstart@1.0.4";
        "string_decoder@1.1.1" = f "string_decoder" "1.1.1" y "9cf1611ba62685d7030ae9e4ba34149c3af03fc8" [
          (s."safe-buffer@~5.1.0")
          ];
        "string_decoder@1.3.0" = f "string_decoder" "1.3.0" y "42f114594a46cf1a8e30b0a84f56c78c3edac21e" [
          (s."safe-buffer@~5.2.0")
          ];
        "string_decoder@^1.0.0" = s."string_decoder@1.3.0";
        "string_decoder@^1.1.1" = s."string_decoder@1.3.0";
        "string_decoder@~1.1.1" = s."string_decoder@1.1.1";
        "strip-ansi@3.0.1" = f "strip-ansi" "3.0.1" y "6a385fb8853d952d5ff05d0e8aaf94278dc63dcf" [
          (s."ansi-regex@^2.0.0")
          ];
        "strip-ansi@6.0.1" = f "strip-ansi" "6.0.1" y "9e26c63d30f53443e9489495b2105d37b67a85d9" [
          (s."ansi-regex@^5.0.1")
          ];
        "strip-ansi@^3.0.1" = s."strip-ansi@3.0.1";
        "strip-ansi@^6.0.0" = s."strip-ansi@6.0.1";
        "strip-ansi@^6.0.1" = s."strip-ansi@6.0.1";
        "strip-eof@1.0.0" = f "strip-eof" "1.0.0" y "bb43ff5598a6eb05d89b59fcd129c983313606bf" [];
        "strip-eof@^1.0.0" = s."strip-eof@1.0.0";
        "strip-indent@3.0.0" = f "strip-indent" "3.0.0" y "c32e1cee940b6b3432c771bc2c54bcce73cd3001" [
          (s."min-indent@^1.0.0")
          ];
        "strip-indent@^3.0.0" = s."strip-indent@3.0.0";
        "style-loader@1.3.0" = f "style-loader" "1.3.0" y "828b4a3b3b7e7aa5847ce7bae9e874512114249e" [
          (s."loader-utils@^2.0.0")
          (s."schema-utils@^2.7.0")
          ];
        "style-loader@^1.3.0" = s."style-loader@1.3.0";
        "style-to-object@0.3.0" = f "style-to-object" "0.3.0" y "b1b790d205991cc783801967214979ee19a76e46" [
          (s."inline-style-parser@0.1.1")
          ];
        "style-to-object@^0.3.0" = s."style-to-object@0.3.0";
        "supports-color@5.5.0" = f "supports-color" "5.5.0" y "e2e69a44ac8772f78a1ec0b35b689df6530efc8f" [
          (s."has-flag@^3.0.0")
          ];
        "supports-color@7.2.0" = f "supports-color" "7.2.0" y "1b7dcdcb32b8138801b3e478ba6a51caa89648da" [
          (s."has-flag@^4.0.0")
          ];
        "supports-color@^5.3.0" = s."supports-color@5.5.0";
        "supports-color@^7.0.0" = s."supports-color@7.2.0";
        "supports-color@^7.1.0" = s."supports-color@7.2.0";
        "supports-preserve-symlinks-flag@1.0.0" = f "supports-preserve-symlinks-flag" "1.0.0" y "6eda4bd344a3c94aea376d4cc31bc77311039e09" [];
        "supports-preserve-symlinks-flag@^1.0.0" = s."supports-preserve-symlinks-flag@1.0.0";
        "symbol.prototype.description@1.0.5" = f "symbol.prototype.description" "1.0.5" y "d30e01263b6020fbbd2d2884a6276ce4d49ab568" [
          (s."call-bind@^1.0.2")
          (s."get-symbol-description@^1.0.0")
          (s."has-symbols@^1.0.2")
          (s."object.getownpropertydescriptors@^2.1.2")
          ];
        "symbol.prototype.description@^1.0.0" = s."symbol.prototype.description@1.0.5";
        "synchronous-promise@2.0.15" = f "synchronous-promise" "2.0.15" y "07ca1822b9de0001f5ff73595f3d08c4f720eb8e" [];
        "synchronous-promise@^2.0.15" = s."synchronous-promise@2.0.15";
        "tapable@1.1.3" = f "tapable" "1.1.3" y "a1fccc06b58db61fd7a45da2da44f5f3a3e67ba2" [];
        "tapable@^1.0.0" = s."tapable@1.1.3";
        "tapable@^1.1.3" = s."tapable@1.1.3";
        "tar@6.1.11" = f "tar" "6.1.11" y "6760a38f003afa1b2ffd0ffe9e9abbd0eab3d621" [
          (s."chownr@^2.0.0")
          (s."fs-minipass@^2.0.0")
          (s."minipass@^3.0.0")
          (s."minizlib@^2.1.1")
          (s."mkdirp@^1.0.3")
          (s."yallist@^4.0.0")
          ];
        "tar@^6.0.2" = s."tar@6.1.11";
        "telejson@5.3.3" = f "telejson" "5.3.3" y "fa8ca84543e336576d8734123876a9f02bf41d2e" [
          (s."@types/is-function@^1.0.0")
          (s."global@^4.4.0")
          (s."is-function@^1.0.2")
          (s."is-regex@^1.1.2")
          (s."is-symbol@^1.0.3")
          (s."isobject@^4.0.0")
          (s."lodash@^4.17.21")
          (s."memoizerific@^1.11.3")
          ];
        "telejson@^5.3.2" = s."telejson@5.3.3";
        "telejson@^5.3.3" = s."telejson@5.3.3";
        "terser-webpack-plugin@1.4.5" = f "terser-webpack-plugin" "1.4.5" y "a217aefaea330e734ffacb6120ec1fa312d6040b" [
          (s."cacache@^12.0.2")
          (s."find-cache-dir@^2.1.0")
          (s."is-wsl@^1.1.0")
          (s."schema-utils@^1.0.0")
          (s."serialize-javascript@^4.0.0")
          (s."source-map@^0.6.1")
          (s."terser@^4.1.2")
          (s."webpack-sources@^1.4.0")
          (s."worker-farm@^1.7.0")
          ];
        "terser-webpack-plugin@4.2.3" = f "terser-webpack-plugin" "4.2.3" y "28daef4a83bd17c1db0297070adc07fc8cfc6a9a" [
          (s."cacache@^15.0.5")
          (s."find-cache-dir@^3.3.1")
          (s."jest-worker@^26.5.0")
          (s."p-limit@^3.0.2")
          (s."schema-utils@^3.0.0")
          (s."serialize-javascript@^5.0.1")
          (s."source-map@^0.6.1")
          (s."terser@^5.3.4")
          (s."webpack-sources@^1.4.3")
          ];
        "terser-webpack-plugin@^1.4.3" = s."terser-webpack-plugin@1.4.5";
        "terser-webpack-plugin@^4.2.3" = s."terser-webpack-plugin@4.2.3";
        "terser@4.8.0" = f "terser" "4.8.0" y "63056343d7c70bb29f3af665865a46fe03a0df17" [
          (s."commander@^2.20.0")
          (s."source-map@~0.6.1")
          (s."source-map-support@~0.5.12")
          ];
        "terser@5.10.0" = f "terser" "5.10.0" y "b86390809c0389105eb0a0b62397563096ddafcc" [
          (s."commander@^2.20.0")
          (s."source-map@~0.7.2")
          (s."source-map-support@~0.5.20")
          ];
        "terser@^4.1.2" = s."terser@4.8.0";
        "terser@^4.6.3" = s."terser@4.8.0";
        "terser@^5.3.4" = s."terser@5.10.0";
        "test-exclude@6.0.0" = f "test-exclude" "6.0.0" y "04a8698661d805ea6fa293b6cb9e63ac044ef15e" [
          (s."@istanbuljs/schema@^0.1.2")
          (s."glob@^7.1.4")
          (s."minimatch@^3.0.4")
          ];
        "test-exclude@^6.0.0" = s."test-exclude@6.0.0";
        "throttle-debounce@3.0.1" = f "throttle-debounce" "3.0.1" y "32f94d84dfa894f786c9a1f290e7a645b6a19abb" [];
        "throttle-debounce@^3.0.1" = s."throttle-debounce@3.0.1";
        "through2@2.0.5" = f "through2" "2.0.5" y "01c1e39eb31d07cb7d03a96a70823260b23132cd" [
          (s."readable-stream@~2.3.6")
          (s."xtend@~4.0.1")
          ];
        "through2@^2.0.0" = s."through2@2.0.5";
        "timers-browserify@2.0.12" = f "timers-browserify" "2.0.12" y "44a45c11fbf407f34f97bccd1577c652361b00ee" [
          (s."setimmediate@^1.0.4")
          ];
        "timers-browserify@^2.0.4" = s."timers-browserify@2.0.12";
        "tmpl@1.0.5" = f "tmpl" "1.0.5" y "8683e0b902bb9c20c4f726e3c0b69f36518c07cc" [];
        "to-arraybuffer@1.0.1" = f "to-arraybuffer" "1.0.1" y "7d229b1fcc637e466ca081180836a7aabff83f43" [];
        "to-arraybuffer@^1.0.0" = s."to-arraybuffer@1.0.1";
        "to-fast-properties@2.0.0" = f "to-fast-properties" "2.0.0" y "dc5e698cbd079265bc73e0377681a4e4e83f616e" [];
        "to-fast-properties@^2.0.0" = s."to-fast-properties@2.0.0";
        "to-object-path@0.3.0" = f "to-object-path" "0.3.0" y "297588b7b0e7e0ac08e04e672f85c1f4999e17af" [
          (s."kind-of@^3.0.2")
          ];
        "to-object-path@^0.3.0" = s."to-object-path@0.3.0";
        "to-regex-range@2.1.1" = f "to-regex-range" "2.1.1" y "7c80c17b9dfebe599e27367e0d4dd5590141db38" [
          (s."is-number@^3.0.0")
          (s."repeat-string@^1.6.1")
          ];
        "to-regex-range@5.0.1" = f "to-regex-range" "5.0.1" y "1648c44aae7c8d988a326018ed72f5b4dd0392e4" [
          (s."is-number@^7.0.0")
          ];
        "to-regex-range@^2.1.0" = s."to-regex-range@2.1.1";
        "to-regex-range@^5.0.1" = s."to-regex-range@5.0.1";
        "to-regex@3.0.2" = f "to-regex" "3.0.2" y "13cfdd9b336552f30b51f33a8ae1b42a7a7599ce" [
          (s."define-property@^2.0.2")
          (s."extend-shallow@^3.0.2")
          (s."regex-not@^1.0.2")
          (s."safe-regex@^1.1.0")
          ];
        "to-regex@^3.0.1" = s."to-regex@3.0.2";
        "to-regex@^3.0.2" = s."to-regex@3.0.2";
        "toggle-selection@1.0.6" = f "toggle-selection" "1.0.6" y "6e45b1263f2017fa0acc7d89d78b15b8bf77da32" [];
        "toggle-selection@^1.0.6" = s."toggle-selection@1.0.6";
        "toidentifier@1.0.1" = f "toidentifier" "1.0.1" y "3be34321a88a820ed1bd80dfaa33e479fbb8dd35" [];
        "tr46@0.0.3" = f "tr46" "0.0.3" y "8184fd347dac9cdc185992f3a6622e14b9d9ab6a" [];
        "tr46@~0.0.3" = s."tr46@0.0.3";
        "trim-trailing-lines@1.1.4" = f "trim-trailing-lines" "1.1.4" y "bd4abbec7cc880462f10b2c8b5ce1d8d1ec7c2c0" [];
        "trim-trailing-lines@^1.0.0" = s."trim-trailing-lines@1.1.4";
        "trim@0.0.1" = f "trim" "0.0.1" y "5858547f6b290757ee95cccc666fb50084c460dd" [];
        "trough@1.0.5" = f "trough" "1.0.5" y "b8b639cefad7d0bb2abd37d433ff8293efa5f406" [];
        "trough@^1.0.0" = s."trough@1.0.5";
        "ts-dedent@2.2.0" = f "ts-dedent" "2.2.0" y "39e4bd297cd036292ae2394eb3412be63f563bb5" [];
        "ts-dedent@^2.0.0" = s."ts-dedent@2.2.0";
        "ts-pnp@1.2.0" = f "ts-pnp" "1.2.0" y "a500ad084b0798f1c3071af391e65912c86bca92" [];
        "ts-pnp@^1.1.6" = s."ts-pnp@1.2.0";
        "tslib@2.3.1" = f "tslib" "2.3.1" y "e8a335add5ceae51aa261d32a490158ef042ef01" [];
        "tslib@^2.0.0" = s."tslib@2.3.1";
        "tslib@^2.0.1" = s."tslib@2.3.1";
        "tslib@^2.0.3" = s."tslib@2.3.1";
        "tslib@^2.3.0" = s."tslib@2.3.1";
        "tty-browserify@0.0.0" = f "tty-browserify" "0.0.0" y "a157ba402da24e9bf957f9aa69d524eed42901a6" [];
        "type-check@0.3.2" = f "type-check" "0.3.2" y "5884cab512cf1d355e3fb784f30804b2b520db72" [
          (s."prelude-ls@~1.1.2")
          ];
        "type-check@~0.3.2" = s."type-check@0.3.2";
        "type-fest@0.20.2" = f "type-fest" "0.20.2" y "1bf207f4b28f91583666cb5fbd327887301cd5f4" [];
        "type-fest@0.6.0" = f "type-fest" "0.6.0" y "8d2a2370d3df886eb5c90ada1c5bf6188acf838b" [];
        "type-fest@0.8.1" = f "type-fest" "0.8.1" y "09e249ebde851d3b1e48d27c105444667f17b83d" [];
        "type-fest@^0.20.2" = s."type-fest@0.20.2";
        "type-fest@^0.6.0" = s."type-fest@0.6.0";
        "type-fest@^0.8.1" = s."type-fest@0.8.1";
        "type-is@1.6.18" = f "type-is" "1.6.18" y "4e552cd05df09467dcbc4ef739de89f2cf37c131" [
          (s."media-typer@0.3.0")
          (s."mime-types@~2.1.24")
          ];
        "type-is@~1.6.18" = s."type-is@1.6.18";
        "typedarray-to-buffer@3.1.5" = f "typedarray-to-buffer" "3.1.5" y "a97ee7a9ff42691b9f783ff1bc5112fe3fca9080" [
          (s."is-typedarray@^1.0.0")
          ];
        "typedarray-to-buffer@^3.1.5" = s."typedarray-to-buffer@3.1.5";
        "typedarray@0.0.6" = f "typedarray" "0.0.6" y "867ac74e3864187b1d3d47d996a78ec5c8830777" [];
        "typedarray@^0.0.6" = s."typedarray@0.0.6";
        "uglify-js@3.15.0" = f "uglify-js" "3.15.0" y "2d6a689d94783cab43975721977a13c2afec28f1" [];
        "uglify-js@^3.1.4" = s."uglify-js@3.15.0";
        "unbox-primitive@1.0.1" = f "unbox-primitive" "1.0.1" y "085e215625ec3162574dc8859abee78a59b14471" [
          (s."function-bind@^1.1.1")
          (s."has-bigints@^1.0.1")
          (s."has-symbols@^1.0.2")
          (s."which-boxed-primitive@^1.0.2")
          ];
        "unbox-primitive@^1.0.1" = s."unbox-primitive@1.0.1";
        "unfetch@4.2.0" = f "unfetch" "4.2.0" y "7e21b0ef7d363d8d9af0fb929a5555f6ef97a3be" [];
        "unfetch@^4.2.0" = s."unfetch@4.2.0";
        "unherit@1.1.3" = f "unherit" "1.1.3" y "6c9b503f2b41b262330c80e91c8614abdaa69c22" [
          (s."inherits@^2.0.0")
          (s."xtend@^4.0.0")
          ];
        "unherit@^1.0.4" = s."unherit@1.1.3";
        "unicode-canonical-property-names-ecmascript@2.0.0" = f "unicode-canonical-property-names-ecmascript" "2.0.0" y "301acdc525631670d39f6146e0e77ff6bbdebddc" [];
        "unicode-canonical-property-names-ecmascript@^2.0.0" = s."unicode-canonical-property-names-ecmascript@2.0.0";
        "unicode-match-property-ecmascript@2.0.0" = f "unicode-match-property-ecmascript" "2.0.0" y "54fd16e0ecb167cf04cf1f756bdcc92eba7976c3" [
          (s."unicode-canonical-property-names-ecmascript@^2.0.0")
          (s."unicode-property-aliases-ecmascript@^2.0.0")
          ];
        "unicode-match-property-ecmascript@^2.0.0" = s."unicode-match-property-ecmascript@2.0.0";
        "unicode-match-property-value-ecmascript@2.0.0" = f "unicode-match-property-value-ecmascript" "2.0.0" y "1a01aa57247c14c568b89775a54938788189a714" [];
        "unicode-match-property-value-ecmascript@^2.0.0" = s."unicode-match-property-value-ecmascript@2.0.0";
        "unicode-property-aliases-ecmascript@2.0.0" = f "unicode-property-aliases-ecmascript" "2.0.0" y "0a36cb9a585c4f6abd51ad1deddb285c165297c8" [];
        "unicode-property-aliases-ecmascript@^2.0.0" = s."unicode-property-aliases-ecmascript@2.0.0";
        "unified@9.2.0" = f "unified" "9.2.0" y "67a62c627c40589edebbf60f53edfd4d822027f8" [
          (s."bail@^1.0.0")
          (s."extend@^3.0.0")
          (s."is-buffer@^2.0.0")
          (s."is-plain-obj@^2.0.0")
          (s."trough@^1.0.0")
          (s."vfile@^4.0.0")
          ];
        "union-value@1.0.1" = f "union-value" "1.0.1" y "0b6fe7b835aecda61c6ea4d4f02c14221e109847" [
          (s."arr-union@^3.1.0")
          (s."get-value@^2.0.6")
          (s."is-extendable@^0.1.1")
          (s."set-value@^2.0.1")
          ];
        "union-value@^1.0.0" = s."union-value@1.0.1";
        "unique-filename@1.1.1" = f "unique-filename" "1.1.1" y "1d69769369ada0583103a1e6ae87681b56573230" [
          (s."unique-slug@^2.0.0")
          ];
        "unique-filename@^1.1.1" = s."unique-filename@1.1.1";
        "unique-slug@2.0.2" = f "unique-slug" "2.0.2" y "baabce91083fc64e945b0f3ad613e264f7cd4e6c" [
          (s."imurmurhash@^0.1.4")
          ];
        "unique-slug@^2.0.0" = s."unique-slug@2.0.2";
        "unist-builder@2.0.3" = f "unist-builder" "2.0.3" y "77648711b5d86af0942f334397a33c5e91516436" [];
        "unist-builder@^2.0.0" = s."unist-builder@2.0.3";
        "unist-util-generated@1.1.6" = f "unist-util-generated" "1.1.6" y "5ab51f689e2992a472beb1b35f2ce7ff2f324d4b" [];
        "unist-util-generated@^1.0.0" = s."unist-util-generated@1.1.6";
        "unist-util-is@4.1.0" = f "unist-util-is" "4.1.0" y "976e5f462a7a5de73d94b706bac1b90671b57797" [];
        "unist-util-is@^4.0.0" = s."unist-util-is@4.1.0";
        "unist-util-position@3.1.0" = f "unist-util-position" "3.1.0" y "1c42ee6301f8d52f47d14f62bbdb796571fa2d47" [];
        "unist-util-position@^3.0.0" = s."unist-util-position@3.1.0";
        "unist-util-remove-position@2.0.1" = f "unist-util-remove-position" "2.0.1" y "5d19ca79fdba712301999b2b73553ca8f3b352cc" [
          (s."unist-util-visit@^2.0.0")
          ];
        "unist-util-remove-position@^2.0.0" = s."unist-util-remove-position@2.0.1";
        "unist-util-remove@2.1.0" = f "unist-util-remove" "2.1.0" y "b0b4738aa7ee445c402fda9328d604a02d010588" [
          (s."unist-util-is@^4.0.0")
          ];
        "unist-util-remove@^2.0.0" = s."unist-util-remove@2.1.0";
        "unist-util-stringify-position@2.0.3" = f "unist-util-stringify-position" "2.0.3" y "cce3bfa1cdf85ba7375d1d5b17bdc4cada9bd9da" [
          (s."@types/unist@^2.0.2")
          ];
        "unist-util-stringify-position@^2.0.0" = s."unist-util-stringify-position@2.0.3";
        "unist-util-visit-parents@3.1.1" = f "unist-util-visit-parents" "3.1.1" y "65a6ce698f78a6b0f56aa0e88f13801886cdaef6" [
          (s."@types/unist@^2.0.0")
          (s."unist-util-is@^4.0.0")
          ];
        "unist-util-visit-parents@^3.0.0" = s."unist-util-visit-parents@3.1.1";
        "unist-util-visit@2.0.3" = f "unist-util-visit" "2.0.3" y "c3703893146df47203bb8a9795af47d7b971208c" [
          (s."@types/unist@^2.0.0")
          (s."unist-util-is@^4.0.0")
          (s."unist-util-visit-parents@^3.0.0")
          ];
        "unist-util-visit@^2.0.0" = s."unist-util-visit@2.0.3";
        "universalify@2.0.0" = f "universalify" "2.0.0" y "75a4984efedc4b08975c5aeb73f530d02df25717" [];
        "universalify@^2.0.0" = s."universalify@2.0.0";
        "unpipe@1.0.0" = f "unpipe" "1.0.0" y "b2bf4ee8514aae6165b4817829d21b2ef49904ec" [];
        "unpipe@~1.0.0" = s."unpipe@1.0.0";
        "unset-value@1.0.0" = f "unset-value" "1.0.0" y "8376873f7d2335179ffb1e6fc3a8ed0dfc8ab559" [
          (s."has-value@^0.3.1")
          (s."isobject@^3.0.0")
          ];
        "unset-value@^1.0.0" = s."unset-value@1.0.0";
        "upath@1.2.0" = f "upath" "1.2.0" y "8f66dbcd55a883acdae4408af8b035a5044c1894" [];
        "upath@^1.1.1" = s."upath@1.2.0";
        "uri-js@4.4.1" = f "uri-js" "4.4.1" y "9b1a52595225859e55f669d928f88c6c57f2a77e" [
          (s."punycode@^2.1.0")
          ];
        "uri-js@^4.2.2" = s."uri-js@4.4.1";
        "urix@0.1.0" = f "urix" "0.1.0" y "da937f7a62e21fec1fd18d49b35c2935067a6c72" [];
        "urix@^0.1.0" = s."urix@0.1.0";
        "url-loader@4.1.1" = f "url-loader" "4.1.1" y "28505e905cae158cf07c92ca622d7f237e70a4e2" [
          (s."loader-utils@^2.0.0")
          (s."mime-types@^2.1.27")
          (s."schema-utils@^3.0.0")
          ];
        "url-loader@^4.1.1" = s."url-loader@4.1.1";
        "url@0.11.0" = f "url" "0.11.0" y "3838e97cfc60521eb73c525a8e55bfdd9e2e28f1" [
          (s."punycode@1.3.2")
          (s."querystring@0.2.0")
          ];
        "url@^0.11.0" = s."url@0.11.0";
        "use-composed-ref@1.2.1" = f "use-composed-ref" "1.2.1" y "9bdcb5ccd894289105da2325e1210079f56bf849" [];
        "use-composed-ref@^1.0.0" = s."use-composed-ref@1.2.1";
        "use-isomorphic-layout-effect@1.1.1" = f "use-isomorphic-layout-effect" "1.1.1" y "7bb6589170cd2987a152042f9084f9effb75c225" [];
        "use-isomorphic-layout-effect@^1.0.0" = s."use-isomorphic-layout-effect@1.1.1";
        "use-latest@1.2.0" = f "use-latest" "1.2.0" y "a44f6572b8288e0972ec411bdd0840ada366f232" [
          (s."use-isomorphic-layout-effect@^1.0.0")
          ];
        "use-latest@^1.0.0" = s."use-latest@1.2.0";
        "use@3.1.1" = f "use" "3.1.1" y "d50c8cac79a19fbc20f2911f56eb973f4e10070f" [];
        "use@^3.1.0" = s."use@3.1.1";
        "util-deprecate@1.0.2" = f "util-deprecate" "1.0.2" y "450d4dc9fa70de732762fbd2d4a28981419a0ccf" [];
        "util-deprecate@^1.0.1" = s."util-deprecate@1.0.2";
        "util-deprecate@^1.0.2" = s."util-deprecate@1.0.2";
        "util-deprecate@~1.0.1" = s."util-deprecate@1.0.2";
        "util.promisify@1.0.0" = f "util.promisify" "1.0.0" y "440f7165a459c9a16dc145eb8e72f35687097030" [
          (s."define-properties@^1.1.2")
          (s."object.getownpropertydescriptors@^2.0.3")
          ];
        "util@0.10.3" = f "util" "0.10.3" y "7afb1afe50805246489e3db7fe0ed379336ac0f9" [
          (s."inherits@2.0.1")
          ];
        "util@0.11.1" = f "util" "0.11.1" y "3236733720ec64bb27f6e26f421aaa2e1b588d61" [
          (s."inherits@2.0.3")
          ];
        "util@^0.11.0" = s."util@0.11.1";
        "utila@0.4.0" = f "utila" "0.4.0" y "8a16a05d445657a3aea5eecc5b12a4fa5379772c" [];
        "utila@~0.4" = s."utila@0.4.0";
        "utils-merge@1.0.1" = f "utils-merge" "1.0.1" y "9f95710f50a267947b2ccc124741c1028427e713" [];
        "uuid-browser@3.1.0" = f "uuid-browser" "3.1.0" y "0f05a40aef74f9e5951e20efbf44b11871e56410" [];
        "uuid-browser@^3.1.0" = s."uuid-browser@3.1.0";
        "uuid@3.4.0" = f "uuid" "3.4.0" y "b23e4358afa8a202fe7a100af1f5f883f02007ee" [];
        "uuid@^3.3.2" = s."uuid@3.4.0";
        "v8-to-istanbul@8.1.1" = f "v8-to-istanbul" "8.1.1" y "77b752fd3975e31bbcef938f85e9bd1c7a8d60ed" [
          (s."@types/istanbul-lib-coverage@^2.0.1")
          (s."convert-source-map@^1.6.0")
          (s."source-map@^0.7.3")
          ];
        "v8-to-istanbul@^8.0.0" = s."v8-to-istanbul@8.1.1";
        "validate-npm-package-license@3.0.4" = f "validate-npm-package-license" "3.0.4" y "fc91f6b9c7ba15c857f4cb2c5defeec39d4f410a" [
          (s."spdx-correct@^3.0.0")
          (s."spdx-expression-parse@^3.0.0")
          ];
        "validate-npm-package-license@^3.0.1" = s."validate-npm-package-license@3.0.4";
        "vary@1.1.2" = f "vary" "1.1.2" y "2299f02c6ded30d4a5961b0b9f74524a18f634fc" [];
        "vary@~1.1.2" = s."vary@1.1.2";
        "vfile-location@3.2.0" = f "vfile-location" "3.2.0" y "d8e41fbcbd406063669ebf6c33d56ae8721d0f3c" [];
        "vfile-location@^3.0.0" = s."vfile-location@3.2.0";
        "vfile-location@^3.2.0" = s."vfile-location@3.2.0";
        "vfile-message@2.0.4" = f "vfile-message" "2.0.4" y "5b43b88171d409eae58477d13f23dd41d52c371a" [
          (s."@types/unist@^2.0.0")
          (s."unist-util-stringify-position@^2.0.0")
          ];
        "vfile-message@^2.0.0" = s."vfile-message@2.0.4";
        "vfile@4.2.1" = f "vfile" "4.2.1" y "03f1dce28fc625c625bc6514350fbdb00fa9e624" [
          (s."@types/unist@^2.0.0")
          (s."is-buffer@^2.0.0")
          (s."unist-util-stringify-position@^2.0.0")
          (s."vfile-message@^2.0.0")
          ];
        "vfile@^4.0.0" = s."vfile@4.2.1";
        "vm-browserify@1.1.2" = f "vm-browserify" "1.1.2" y "78641c488b8e6ca91a75f511e7a3b32a86e5dda0" [];
        "vm-browserify@^1.0.1" = s."vm-browserify@1.1.2";
        "walker@1.0.8" = f "walker" "1.0.8" y "bd498db477afe573dc04185f011d3ab8a8d7653f" [
          (s."makeerror@1.0.12")
          ];
        "walker@^1.0.7" = s."walker@1.0.8";
        "walker@~1.0.5" = s."walker@1.0.8";
        "warning@4.0.3" = f "warning" "4.0.3" y "16e9e077eb8a86d6af7d64aa1e05fd85b4678ca3" [
          (s."loose-envify@^1.0.0")
          ];
        "warning@^4.0.2" = s."warning@4.0.3";
        "watchpack-chokidar2@2.0.1" = f "watchpack-chokidar2" "2.0.1" y "38500072ee6ece66f3769936950ea1771be1c957" [
          (s."chokidar@^2.1.8")
          ];
        "watchpack-chokidar2@^2.0.1" = s."watchpack-chokidar2@2.0.1";
        "watchpack@1.7.5" = f "watchpack" "1.7.5" y "1267e6c55e0b9b5be44c2023aed5437a2c26c453" [
          (s."graceful-fs@^4.1.2")
          (s."neo-async@^2.5.0")
          (s."chokidar@^3.4.1")
          (s."watchpack-chokidar2@^2.0.1")
          ];
        "watchpack@2.3.1" = f "watchpack" "2.3.1" y "4200d9447b401156eeca7767ee610f8809bc9d25" [
          (s."glob-to-regexp@^0.4.1")
          (s."graceful-fs@^4.1.2")
          ];
        "watchpack@^1.7.4" = s."watchpack@1.7.5";
        "watchpack@^2.2.0" = s."watchpack@2.3.1";
        "web-namespaces@1.1.4" = f "web-namespaces" "1.1.4" y "bc98a3de60dadd7faefc403d1076d529f5e030ec" [];
        "web-namespaces@^1.0.0" = s."web-namespaces@1.1.4";
        "webidl-conversions@3.0.1" = f "webidl-conversions" "3.0.1" y "24534275e2a7bc6be7bc86611cc16ae0a5654871" [];
        "webidl-conversions@^3.0.0" = s."webidl-conversions@3.0.1";
        "webpack-dev-middleware@3.7.3" = f "webpack-dev-middleware" "3.7.3" y "0639372b143262e2b84ab95d3b91a7597061c2c5" [
          (s."memory-fs@^0.4.1")
          (s."mime@^2.4.4")
          (s."mkdirp@^0.5.1")
          (s."range-parser@^1.2.1")
          (s."webpack-log@^2.0.0")
          ];
        "webpack-dev-middleware@^3.7.3" = s."webpack-dev-middleware@3.7.3";
        "webpack-filter-warnings-plugin@1.2.1" = f "webpack-filter-warnings-plugin" "1.2.1" y "dc61521cf4f9b4a336fbc89108a75ae1da951cdb" [];
        "webpack-filter-warnings-plugin@^1.2.1" = s."webpack-filter-warnings-plugin@1.2.1";
        "webpack-hot-middleware@2.25.1" = f "webpack-hot-middleware" "2.25.1" y "581f59edf0781743f4ca4c200fd32c9266c6cf7c" [
          (s."ansi-html-community@0.0.8")
          (s."html-entities@^2.1.0")
          (s."querystring@^0.2.0")
          (s."strip-ansi@^6.0.0")
          ];
        "webpack-hot-middleware@^2.25.1" = s."webpack-hot-middleware@2.25.1";
        "webpack-log@2.0.0" = f "webpack-log" "2.0.0" y "5b7928e0637593f119d32f6227c1e0ac31e1b47f" [
          (s."ansi-colors@^3.0.0")
          (s."uuid@^3.3.2")
          ];
        "webpack-log@^2.0.0" = s."webpack-log@2.0.0";
        "webpack-sources@1.4.3" = f "webpack-sources" "1.4.3" y "eedd8ec0b928fbf1cbfe994e22d2d890f330a933" [
          (s."source-list-map@^2.0.0")
          (s."source-map@~0.6.1")
          ];
        "webpack-sources@^1.4.0" = s."webpack-sources@1.4.3";
        "webpack-sources@^1.4.1" = s."webpack-sources@1.4.3";
        "webpack-sources@^1.4.3" = s."webpack-sources@1.4.3";
        "webpack-virtual-modules@0.2.2" = f "webpack-virtual-modules" "0.2.2" y "20863dc3cb6bb2104729fff951fbe14b18bd0299" [
          (s."debug@^3.0.0")
          ];
        "webpack-virtual-modules@^0.2.2" = s."webpack-virtual-modules@0.2.2";
        "webpack@4" = s."webpack@4.46.0";
        "webpack@4.46.0" = f "webpack" "4.46.0" y "bf9b4404ea20a073605e0a011d188d77cb6ad542" [
          (s."@webassemblyjs/ast@1.9.0")
          (s."@webassemblyjs/helper-module-context@1.9.0")
          (s."@webassemblyjs/wasm-edit@1.9.0")
          (s."@webassemblyjs/wasm-parser@1.9.0")
          (s."acorn@^6.4.1")
          (s."ajv@^6.10.2")
          (s."ajv-keywords@^3.4.1")
          (s."chrome-trace-event@^1.0.2")
          (s."enhanced-resolve@^4.5.0")
          (s."eslint-scope@^4.0.3")
          (s."json-parse-better-errors@^1.0.2")
          (s."loader-runner@^2.4.0")
          (s."loader-utils@^1.2.3")
          (s."memory-fs@^0.4.1")
          (s."micromatch@^3.1.10")
          (s."mkdirp@^0.5.3")
          (s."neo-async@^2.6.1")
          (s."node-libs-browser@^2.2.1")
          (s."schema-utils@^1.0.0")
          (s."tapable@^1.1.3")
          (s."terser-webpack-plugin@^1.4.3")
          (s."watchpack@^1.7.4")
          (s."webpack-sources@^1.4.1")
          ];
        "whatwg-url@5.0.0" = f "whatwg-url" "5.0.0" y "966454e8765462e37644d3626f6742ce8b70965d" [
          (s."tr46@~0.0.3")
          (s."webidl-conversions@^3.0.0")
          ];
        "whatwg-url@^5.0.0" = s."whatwg-url@5.0.0";
        "which-boxed-primitive@1.0.2" = f "which-boxed-primitive" "1.0.2" y "13757bc89b209b049fe5d86430e21cf40a89a8e6" [
          (s."is-bigint@^1.0.1")
          (s."is-boolean-object@^1.1.0")
          (s."is-number-object@^1.0.4")
          (s."is-string@^1.0.5")
          (s."is-symbol@^1.0.3")
          ];
        "which-boxed-primitive@^1.0.2" = s."which-boxed-primitive@1.0.2";
        "which@1.3.1" = f "which" "1.3.1" y "a45043d54f5805316da8d62f9f50918d3da70b0a" [
          (s."isexe@^2.0.0")
          ];
        "which@2.0.2" = f "which" "2.0.2" y "7c6a8dd0a636a0327e10b59c9286eee93f3f51b1" [
          (s."isexe@^2.0.0")
          ];
        "which@^1.2.9" = s."which@1.3.1";
        "which@^2.0.1" = s."which@2.0.2";
        "wide-align@1.1.5" = f "wide-align" "1.1.5" y "df1d4c206854369ecf3c9a4898f1b23fbd9d15d3" [
          (s."string-width@^1.0.2 || 2 || 3 || 4")
          ];
        "wide-align@^1.1.2" = s."wide-align@1.1.5";
        "widest-line@3.1.0" = f "widest-line" "3.1.0" y "8292333bbf66cb45ff0de1603b136b7ae1496eca" [
          (s."string-width@^4.0.0")
          ];
        "widest-line@^3.1.0" = s."widest-line@3.1.0";
        "word-wrap@1.2.3" = f "word-wrap" "1.2.3" y "610636f6b1f703891bd34771ccb17fb93b47079c" [];
        "word-wrap@~1.2.3" = s."word-wrap@1.2.3";
        "wordwrap@1.0.0" = f "wordwrap" "1.0.0" y "27584810891456a4171c8d0226441ade90cbcaeb" [];
        "wordwrap@^1.0.0" = s."wordwrap@1.0.0";
        "worker-farm@1.7.0" = f "worker-farm" "1.7.0" y "26a94c5391bbca926152002f69b84a4bf772e5a8" [
          (s."errno@~0.1.7")
          ];
        "worker-farm@^1.7.0" = s."worker-farm@1.7.0";
        "worker-rpc@0.1.1" = f "worker-rpc" "0.1.1" y "cb565bd6d7071a8f16660686051e969ad32f54d5" [
          (s."microevent.ts@~0.1.1")
          ];
        "worker-rpc@^0.1.0" = s."worker-rpc@0.1.1";
        "wrap-ansi@7.0.0" = f "wrap-ansi" "7.0.0" y "67e145cff510a6a6984bdf1152911d69d2eb9e43" [
          (s."ansi-styles@^4.0.0")
          (s."string-width@^4.1.0")
          (s."strip-ansi@^6.0.0")
          ];
        "wrap-ansi@^7.0.0" = s."wrap-ansi@7.0.0";
        "wrappy@1" = s."wrappy@1.0.2";
        "wrappy@1.0.2" = f "wrappy" "1.0.2" y "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f" [];
        "write-file-atomic@3.0.3" = f "write-file-atomic" "3.0.3" y "56bd5c5a5c70481cd19c571bd39ab965a5de56e8" [
          (s."imurmurhash@^0.1.4")
          (s."is-typedarray@^1.0.0")
          (s."signal-exit@^3.0.2")
          (s."typedarray-to-buffer@^3.1.5")
          ];
        "write-file-atomic@^3.0.0" = s."write-file-atomic@3.0.3";
        "ws@8.4.2" = f "ws" "8.4.2" y "18e749868d8439f2268368829042894b6907aa0b" [];
        "ws@^8.2.3" = s."ws@8.4.2";
        "xtend@4.0.2" = f "xtend" "4.0.2" y "bb72779f5fa465186b1f438f674fa347fdb5db54" [];
        "xtend@^4.0.0" = s."xtend@4.0.2";
        "xtend@^4.0.1" = s."xtend@4.0.2";
        "xtend@~4.0.1" = s."xtend@4.0.2";
        "y18n@4.0.3" = f "y18n" "4.0.3" y "b5f259c82cd6e336921efd7bfd8bf560de9eeedf" [];
        "y18n@5.0.8" = f "y18n" "5.0.8" y "7f4934d0f7ca8c56f95314939ddcd2dd91ce1d55" [];
        "y18n@^4.0.0" = s."y18n@4.0.3";
        "y18n@^5.0.5" = s."y18n@5.0.8";
        "yallist@3.1.1" = f "yallist" "3.1.1" y "dbb7daf9bfd8bac9ab45ebf602b8cbad0d5d08fd" [];
        "yallist@4.0.0" = f "yallist" "4.0.0" y "9bb92790d9c0effec63be73519e11a35019a3a72" [];
        "yallist@^3.0.2" = s."yallist@3.1.1";
        "yallist@^4.0.0" = s."yallist@4.0.0";
        "yaml@1.10.2" = f "yaml" "1.10.2" y "2301c5ffbf12b467de8da2333a459e29e7920e4b" [];
        "yaml@^1.10.0" = s."yaml@1.10.2";
        "yaml@^1.7.2" = s."yaml@1.10.2";
        "yargs-parser@20.2.9" = f "yargs-parser" "20.2.9" y "2eb7dc3b0289718fc295f362753845c41a0c94ee" [];
        "yargs-parser@^20.2.2" = s."yargs-parser@20.2.9";
        "yargs-parser@^20.2.7" = s."yargs-parser@20.2.9";
        "yargs@16.2.0" = f "yargs" "16.2.0" y "1c82bf0f6b6a66eafce7ef30e376f49a12477f66" [
          (s."cliui@^7.0.2")
          (s."escalade@^3.1.1")
          (s."get-caller-file@^2.0.5")
          (s."require-directory@^2.1.1")
          (s."string-width@^4.2.0")
          (s."y18n@^5.0.5")
          (s."yargs-parser@^20.2.2")
          ];
        "yargs@^16.2.0" = s."yargs@16.2.0";
        "yocto-queue@0.1.0" = f "yocto-queue" "0.1.0" y "0294eb3dee05028d31ee1a5fa2c556a6aaf10a1b" [];
        "yocto-queue@^0.1.0" = s."yocto-queue@0.1.0";
        "zwitch@1.0.5" = f "zwitch" "1.0.5" y "d11d7381ffed16b742f6af7b3f223d5cd9fe9920" [];
        "zwitch@^1.0.0" = s."zwitch@1.0.5";
        }