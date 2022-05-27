let inherit (builtins) fromJSON readFile;readJson = x: fromJSON (readFile x); in
{
  circles-pink-state-machine = { spagoPkgs = import ./circles-pink-state-machine/spago-packages.nix; meta = readJson ./circles-pink-state-machine/meta.json; };
}
