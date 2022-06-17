{
  chance = {
    spagoPkgs = import ./chance/spago-packages.nix;
    meta = builtins.fromJSON (builtins.readFile ./chance/meta.json);
    location = ../../pkgs/purs/chance;
  };
  circles-pink-state-machine = {
    spagoPkgs = import ./circles-pink-state-machine/spago-packages.nix;
    meta = builtins.fromJSON (builtins.readFile ./circles-pink-state-machine/meta.json);
    location = ../../pkgs/purs/circles-pink-state-machine;
  };
  fp-ts = {
    spagoPkgs = import ./fp-ts/spago-packages.nix;
    meta = builtins.fromJSON (builtins.readFile ./fp-ts/meta.json);
    location = ../../pkgs/purs/fp-ts;
  };
}