let
  pkgs = import <nixpkgs> { }; in
{
  foo = pkgs.writeText "hello.txt" "hello circles pink! #3";
}
