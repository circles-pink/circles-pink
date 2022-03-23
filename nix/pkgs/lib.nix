rec {
  mkUrl = opts@{ protocol, subdomain ? null, domain, topLevelDomain }: "${protocol}://${mkDomain opts}";

  mkDomain = { protocol, subdomain ? null, domain, topLevelDomain }:
    let
      sub = if builtins.isString subdomain then "${subdomain}." else "";
    in
    "${sub}${domain}.${topLevelDomain}";
}
