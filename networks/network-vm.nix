{ pkgs ? (import ./default.nix).inputs.nixpkgs, ... }:

{

  webserver =
    {
      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 1024; # megabytes
      deployment.virtualbox.vcpu = 2; # number of cpus
      deployment.virtualbox.headless = true;
    };
}
