{ pkgs, ... }: {
  boot = {
    zfs.requestEncryptionCredentials = true;
    zfs.devNodes = "/dev";

    loader.grub = {
      enable = true;
      device = "nodev";
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
  };
}
