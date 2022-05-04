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
      inherit (lib) range concatMapStrings const;

      mapAccumL = "TODO";

      # string -> int -> list string
      replicate = s: n: pipe n [ (range 1) (concatMapStrings (const s)) ];

    in
    { inherit replicate mapAccumL; };

  function =
    let
      identity = x: x;
      const = x: _: x;
      comp = f: g: x: f (g (x));
    in
    {
      inherit identity const comp;
    };

  Apply =
    let
      inherit (function) identity const;

      applySnd = Apply: m1: m2: pipe
        (Apply.map (const identity) m1) [
        (f: Apply.apply f m2)
      ];
    in
    {
      inherit applySnd;
    };

  Bind =
    { };

  io =
    let
      inherit (builtins) seq trace;
      inherit (function) const comp;

      map = f: x: _: let x' = x null; in seq x' (f x');
      bind = f: x: _: let x' = x null; in seq x' (f x' null);
      apply = f: x: _:
        let
          f' = f null;
          x' = x null;
        in
        seq f' (seq x' (f' x'));
      pure = x: _: x;
      bind_ = y: bind (const y);
      map_ = x: map (const x);

      bindAs = n: x: y: bind (s: map (v: s // { ${n} = v; }) (x s)) y;
      drop = bindAs "_";
      drop_ = x: drop (const x);
      letAs = n: x: bindAs (comp pure x);
      applySnd = fp.Apply.applySnd Apply;
      do = pure { };


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
      inherit
        map
        bind
        bindAs
        drop
        drop_
        bind_
        map_
        apply
        pure
        Functor
        Bind
        Apply
        Applicative
        Monad
        log
        letAs
        applySnd
        do;
    };

  fp = {
    inherit either io function Apply type list;
    tests = {
      either = either.tests;
    };
  };

in
fp
