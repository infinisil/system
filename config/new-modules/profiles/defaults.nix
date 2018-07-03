{ config, lib, ... }:

with lib;

{
  time.timeZone = mkDefault "Europe/Zurich";

  mine.mainUsers = [ "infinisil" "root" ];

  networking.nameservers = mkDefault [
    "1.1.1.1"
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
    daemonNiceLevel = 1;

    binaryCaches = [
      "https://r-ryantm.cachix.org"
    ];
    binaryCachePublicKeys = [
      "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
    ];
  };

  nixpkgs = {
    config.allowUnfree = true;

    overlays = [
      (import ../../../external/home-manager/overlay.nix)
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  boot.cleanTmpDir = true;

}
