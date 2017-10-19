{ lib, ... }:

with lib;

let
  single = n: device:
    let
      fs = "/boot/${toString n}";
    in {
      fileSystems.${fs} = mkForce {
        inherit device;
        fsType = "vfat";
        options = [
          "x-systemd.automount"
          "x-systemd.device-timeout=10"
        ];
      };

      boot.loader.grub.mirroredBoots = [{
        devices = [ "nodev" ];
        path = fs;
      }];
    };

  boots = imap single [
    "/dev/disk/by-uuid/20AA-D17C"
    "/dev/disk/by-uuid/2114-63F3"
    "/dev/disk/by-uuid/20EB-DFC7"
  ];
in
{
  imports = [
    ./hardware.nix
  ] ++ boots;

  boot = {
    cleanTmpDir = true;
    zfs.devNodes = "/dev/mapper";

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
      splashImage = null;
      efiSupport = true;
      efiInstallAsRemovable = true;
    };

    supportedFilesystems = [ "exfat" ];
  };
}
