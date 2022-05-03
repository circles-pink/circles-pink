{ lib }:
let
  inherit (lib) pipe;
  assertEq = expr: expected: { inherit expr expected; };
  inherit (type) tag match;

  type =
    let
      tag = type: data: { inherit type data; };
      match = cases: x: cases.${x.type} x.data;
    in
    { inherit tag match; };


  either =
    let
      # Constructors

      left = tag "left";
      right = tag "right";

      #

      map = f: match { left = left; right = x: right (f x); };
      bind = f: match { left = left; right = f; };

      # Instances

      Functor = { inherit map; };
      Bind = { inherit bind; };
      #Apply = Functor // { inherit apply; };
      #Applicative = Apply // { inherit pure; };
      #Monad = Applicative // Bind;
    in
    {
      inherit left right map bind;
      tests = {
        # testLeft = assertEq
        #   (pipe (left 1) [ (match { left = identity; right = const 0; }) ])
        #   1;
        # testRight = assertEq
        #   (pipe (right 1) [ (match { left = const 0; right = identity; }) ])
        #   1;
      };
    };

  list =
    let
      mapAccumL = "TODO";

      # string -> int -> list string
      replicate = s: n: pipe n [ (range 1) (concatMapStrings (const s)) ];

    in
    { };

  function =
    let
      identity = x: x;
      const = _: x: x;
    in
    {
      inherit identity const;
    };

  Apply =
    let
      inherit (function) identity const;

      applySnd = Apply: m1: m2: pipe
        (Apply.map (const identity) m1) [
        (f: Apply.apply f m2)
      ];
    in
    { };

  Bind =
    { };

  io =
    let
      inherit (builtins) seq trace;

      map = f: x: _: f (x null); # seq, too?
      bind = f: x: _: let r = x null; in seq r (f r null);
      apply = f: x: _: (f null) (x null); # seq, too?
      pure = x: _: x;
      bind_ = y: x: _: let r = x null; in seq r (y null);

      # Instances

      Functor = { inherit map; };
      Bind = { inherit bind; };
      Apply = Functor // { inherit apply; };
      Applicative = Apply // { inherit pure; };
      Monad = Applicative // Bind;

      # Util

      log = s: _: trace s null;
    in
    {
      inherit map bind apply pure Functor Bind Apply Applicative Monad log;
    };

in
{
  inherit either io function Apply type;
  tests = {
    either = either.tests;
  };
}
