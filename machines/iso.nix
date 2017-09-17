{ config, pkgs, ...}:
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    ../modules/keylayout.nix
  ];

  boot.supportedFilesystems = [ "zfs" ];
}
