{ pkgs, ... }:

{

  imports = [
    ../modules/ch.nix
    ../modules/keys.nix
    ../lib
    ../private
    ../home-manager/nixos
  ];

  users.extraUsers.infinisil = {
    description = "Silvan Mosberger";
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "systemd-journal"
    ];
  };

  nix = {
    useSandbox = true;
    buildCores = 0;
    autoOptimiseStore = true;
    trustedUsers = [ "root" "@wheel" ];
  };

  nixpkgs.config.allowUnfree = true;

  security.sudo.wheelNeedsPassword = false;

  boot.loader.grub.splashImage = null;

  boot.cleanTmpDir = true;

}
