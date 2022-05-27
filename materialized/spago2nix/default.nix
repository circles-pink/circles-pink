let inherit (builtins) fromJSON readFile; readJson = x: fromJSON (readFile x); in
{
  chance = { spagoPkgs = import ./chance/spago-packages.nix; meta = readJson ./chance/meta.json; sources = ../../pkgs/purs/chance/src; testSources = ../../pkgs/purs/chance/test;}; 
  circles-pink-state-machine = { spagoPkgs = import ./circles-pink-state-machine/spago-packages.nix; meta = readJson ./circles-pink-state-machine/meta.json; sources = ../../pkgs/purs/circles-pink-state-machine/src; testSources = ../../pkgs/purs/circles-pink-state-machine/test;}; 
}
