let inherit (builtins) fromJSON readFile; readJson = x: fromJSON (readFile x); in
{
  chance = { spagoPkgs = import ./chance/spago-packages.nix; meta = readJson ./chance/meta.json; location = ../../pkgs/purs/chance/.;}; 
  circles-pink-state-machine = { spagoPkgs = import ./circles-pink-state-machine/spago-packages.nix; meta = readJson ./circles-pink-state-machine/meta.json; location = ../../pkgs/purs/circles-pink-state-machine/.;}; 
}
