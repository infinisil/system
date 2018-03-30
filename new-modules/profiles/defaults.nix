{ config, lib, ... }:

with lib;

{

  time.timeZone = mkDefault "Europe/Zurich";

  mine.mainUsers = [ "infinisil" "root" ];

  # https://www.quad9.net/
  networking.nameservers = mkDefault [
    "9.9.9.9"
    "149.112.112.112"
    "2620:fe::fe"
  ];

  mine.keylayout.enable = true;

  users.extraUsers.infinisil = {
    description = "Silvan Mosberger";
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "systemd-journal"
    ];
  };

  nix = {
    #package = pkgs.nixUnstable;
    useSandbox = true;
    buildCores = 0;
    autoOptimiseStore = true;
    trustedUsers = [ "root" "@wheel" ];
  };

  nixpkgs = {
    config.allowUnfree = true;

    overlays = [
      (import ../../home-manager/overlay.nix)
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  boot.cleanTmpDir = true;

}
