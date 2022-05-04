{ pkgs, ... }:
let
  inherit (pkgs) nix-fp-lite lib runCommand;
  inherit (lib) pipe flip concatStringsSep;
  inherit (nix-fp-lite) io list Apply type either function;
  inherit (Apply) applySnd;
  inherit (function) comp;
  inherit (type) tag match;
  inherit (either) left right;
  inherit (io) bind_ log pure;
  inherit (list) mapAccumL replicate;
  inherit (lib) todo_ todo const;

  # Test

  # string -> assert -> test
  testCase = name: assertion: tag "testCase" { inherit name assertion; };

  # string -> list test -> test
  testGroup = name: tests: tag "testGroup" { inherit name tests; };

  runTests' =
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

  runTests = tests:
    pipe io.do [
      (io.drop_ (log "Runnig Test suite..."))
      #(bind_ (runTests' 0))
      (io.bindAs "failureCount" (s: pure 0))
      (io.drop (s: (log "${toString s.failureCount} tests failed.")))
      (io.map (s: s.failureCount))
    ];

  runTestsDrv = tests:
    let failedCount = runTests tests null; in
    if failedCount == 0
    then runCommand "empty" { } "mkdir $out"
    else abort "Tests failed";

  # Assert

  # a -> (a -> bool) -> assert
  satisfies = val: pred: msg:
    if pred val
    then right null
    else left [ "Value ${val} does not satisfy: " msg ];

  # a -> assert
  isTrue = val:
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

  log' = comp log (concatStringsSep "\n");

  # int -> list string
  mkIndent = replicate "  ";

in
{
  inherit
    satisfies
    isTrue
    isEq
    isLt
    runTests
    runTestsDrv
    testCase
    testGroup;
}
