{ pkgs, ... }: rec {
  mkUrl = opts@{ protocol, subdomain ? null, domain, topLevelDomain, path ? null }: "${protocol}://${mkDomain opts}";

  mkDomain = { protocol, subdomain ? null, domain, topLevelDomain, path ? null }:
    let
      sub = if builtins.isString subdomain then "${subdomain}." else "";
      path' = if builtins.isString path then path else "";
    in
    "${sub}${domain}.${topLevelDomain}${path'}";

  changeCase.constantCase = str:
    builtins.readFile (pkgs.runJS "constantCase"
      {
        libraries = map pkgs.yarn2nix-to-node2nix [
          pkgs.circles-pink.yarn2nix.yarnPkgs."@circles-pink/change-case"
        ];
      } ''
      const { constantCase } = require("change-case");
      const fs = require("fs");
      fs.writeFileSync(process.env.out, constantCase("${str}"))
    '');

  todo = msg: abort "TODO: ${msg}";

  todo_ = abort "TODO";
}
