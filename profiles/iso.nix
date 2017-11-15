{ config, pkgs, ...}:
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    ./defaults.nix
    ../modules/keylayout.nix
  ];
}
