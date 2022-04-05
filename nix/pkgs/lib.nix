{ pkgs, ... }: rec {
  mkUrl = opts@{ protocol, subdomain ? null, domain, topLevelDomain, path ? null }: "${protocol}://${mkDomain opts}";

  mkDomain = { protocol, subdomain ? null, domain, topLevelDomain, path ? null }:
    let
      sub = if builtins.isString subdomain then "${subdomain}." else "";
      path' = if builtins.isString path then path else "";
    in
    "${sub}${domain}.${topLevelDomain}${path'}";

  changeCase.constantCase = str:
    builtins.readFile (pkgs.runYarnJS "camelCase"
      {
        libraries = [
          pkgs.circles-pink.yarn2nix.yarnPkgs."@circles-pink/change-case"
        ];
      } ''
      const { constantCase } = require("change-case");
      const fs = require("fs");
      fs.writeFileSync(process.env.out, constantCase("${str}"))
    '');
}
