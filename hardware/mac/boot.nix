{ lib, ... }:

with lib;

{
  boot = {
    zfs = {
      devNodes = "/dev";
      enableUnstable = true;
    };

    loader.grub = {
      enable = true;
      device = "nodev";
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
  };
}
