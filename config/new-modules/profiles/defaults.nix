{ lib, ... }: {

  mine.mainUsers = [ "root" ];

  networking.nameservers = lib.mkDefault [ "1.1.1.1" ];

  nix.settings.trusted-users = [ "root" "@wheel" ];
  nixpkgs.config.allowUnfree = true;

  home-manager.useUserPackages = true;

  security.sudo.wheelNeedsPassword = false;

  boot.cleanTmpDir = true;

}
