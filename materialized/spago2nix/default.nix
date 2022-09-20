{
  all-tests = {
    spagoPkgs = import ./all-tests/spago-packages.nix;
    meta = builtins.fromJSON (builtins.readFile ./all-tests/meta.json);
    location = ../../pkgs/purs/all-tests;
  };
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
  debug-extra = {
    spagoPkgs = import ./debug-extra/spago-packages.nix;
    meta = builtins.fromJSON (builtins.readFile ./debug-extra/meta.json);
    location = ../../pkgs/purs/debug-extra;
  };
  graph = {
    spagoPkgs = import ./graph/spago-packages.nix;
    meta = builtins.fromJSON (builtins.readFile ./graph/meta.json);
    location = ../../pkgs/purs/graph;
  };
  indexed-graph = {
    spagoPkgs = import ./indexed-graph/spago-packages.nix;
    meta = builtins.fromJSON (builtins.readFile ./indexed-graph/meta.json);
    location = ../../pkgs/purs/indexed-graph;
  };
}