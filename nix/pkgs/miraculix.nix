{ pkgs, ... }:
let
  inherit (pkgs) nix-fp-lite;
  inherit (nix-fp-lite) io;
  inherit (nix-fp-lite.Apply) applySnd;
  inherit (nix-fp-lite.type) tag;

  # Test

  # string -> assert -> test
  testCase = name: assertion: tag "testCase" { inherit name assertion; };

  # string -> list test -> test
  testGroup = name: tests: tag "testGroup" { inherit name tests; };

  runTests' =
    let
      inherit (io) bind_;
      inherit (list) mapAccumL;
    in
    depth: match {
      test = { name, assertion }: flip match assertion {
        left = _: log' [ "${name}: ok" ];
        right = msg: log' ([ "${name}: failed" ] ++ msg);
      };

      group = { name, tests }: pipe
        (log [ name ]) [
        (bind_ (mapAccumL (runTests' depth) 0 tests))
      ];
    };

  runTests =
    let inherit (io) bind_; in
    pipe
      (log' [ "Runnig Test suite..." ]) [
      (bind (_: 1))
      (bind_ A (runTests' 0))
      (bind_ A (log' [ "Runnig Test suite..." ]))
    ];

  runTestsDrv = "TODO";

  # Assert

  # a -> (a -> bool) -> assert
  satisfies = val: pred: msg:
    if pred val
    then right null
    else left [ "Value ${val} does not satisfy: " msg ];

  # a -> assert
  isTrue = actual:
    if val
    then right null
    else left [ "Value ${val} is not true" ];

  # a -> a -> assert
  isEq = actual: expected:
    if actual == expected
    then right null
    else left [ "Actual value" actual "does not equal expected value" expected ];

  # a -> a -> assert
  isLt = actual: expected:
    if actual < expected
    then right null
    else [ "Actual value" actual "is not lower than value" expected ];

  # Util

  log' = "TODO";

  # int -> list string
  mkIndent = replicate "  ";

in
{ satisfies
, isTrue
, isEq
, isLt
, runTests
, runTestsDrv
, testCase
, testGroup
}
