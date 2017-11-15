{ lib, ... }:

with lib;

{

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/20EB-DFC7";
    fsType = "vfat";
    options = [
      "x-systemd.automount"
      "x-systemd.device-timeout=10"
    ];
  };

  boot = {
    zfs = {
      devNodes = "/dev/mapper";
      enableUnstable = true;
    };

    initrd.luks.devices = {
      key = {
        device = "/dev/disk/by-partlabel/key";
      };
      root = {
        device = "/dev/disk/by-uuid/bcf2dd01-ef96-412a-a458-0bd0437cd83a";
        keyFile = "/dev/mapper/key";
      };
      swap = {
        device = "/dev/disk/by-uuid/59515706-b6b7-4823-95e3-b1d930aca2f8";
        keyFile = "/dev/mapper/key";
      };
    };

    loader.grub = {
      enable = true;
      device = "nodev";
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
  };
}
