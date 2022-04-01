rec {
  mkUrl = opts@{ protocol, subdomain ? null, domain, topLevelDomain, path ? null }: "${protocol}://${mkDomain opts}";

  mkDomain = { protocol, subdomain ? null, domain, topLevelDomain, path ? null }:
    let
      sub = if builtins.isString subdomain then "${subdomain}." else "";
      path' = if builtins.isString path then path else "";
    in
    "${sub}${domain}.${topLevelDomain}${path'}";
}
