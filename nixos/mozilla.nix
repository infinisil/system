{ config, pkgs, ... }:

# Custom packages
let
  mozillaPkgs = import /home/infinisil/src/nixpkgs-mozilla {};
in
{
  environment.systemPackages = [
    #mozillaPkgs.firefox-nightly-bin
  ];
}
