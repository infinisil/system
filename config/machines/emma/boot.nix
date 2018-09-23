{ pkgs, ... }: {
  boot = {
    zfs = {
      devNodes = "/dev";
    };

    loader.grub = {
      enable = true;
      device = "nodev";
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
    kernelPackages = pkgs.linuxPackages_latest;
  };
}
