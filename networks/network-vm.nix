{ pkgs ? (import ../default.nix).outputs.packages.x86_64-linux.pkgs, ... }:

{

  webserver =
    {
      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 1024; # megabytes
      deployment.virtualbox.vcpu = 2; # number of cpus
      deployment.virtualbox.headless = true;
    };
}
