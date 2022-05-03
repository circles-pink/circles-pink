{ pkgs, ... }:

let

  inherit (builtins) toJSON trace readFile;
  inherit (pkgs.lib)
    foldr
    nameValuePair
    pipe
    filterAttrs
    mapAttrsToList
    listToAttrs
    hasPrefix
    isAttrs
    mapAttrs
    length
    runTests;
  inherit (pkgs) writeText runCommand;


  doTests = as:
    let
      failures = runTests as;
      failureCount = length failures;
      report = pipe
        (toJSON failures) [
        (writeText "report.json")
        (p: runCommand "format" { } "cat ${p} | ${pkgs.jq}/bin/jq > $out")
        readFile
        (txt: "\n${txt}\n")
      ];
      out =
        if failureCount != 0 then
          abort "${toString failureCount} tests failed"
        else runCommand "out" { } "mkdir $out";
    in
    trace report out;

  doTestSuite = mapAttrs (name: value: pipe (doTests value) [ (trace "Test: ${name}") ]);

in

{

  pursTests = pkgs.circles-pink.purs.projectTests;

  nixLint =
    let
      # This is only the beginning, in the end all nix files should be linted
      paths = [
        ./pkgs/ts.nix
        ./checks.nix
      ];
    in
    pkgs.runCommand "nix-lint" { } ''
      ${pkgs.deadnix}/bin/deadnix --fail ${builtins.concatStringsSep " " paths}
      mkdir $out
    '';

  nixUnitTests = runTestsDrv (testGroup "group" [
    (testCase "test" (isEq 1 2))
  ]);

} // (doTestSuite pkgs.nix-fp.tests)
