{ options, config, lib, ... }:

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

    binaryCaches = options.nix.binaryCaches.default ++ [
      "https://r-ryantm.cachix.org"
      "https://cachix.cachix.org"
      "https://hie-nix.cachix.org"
    ];
    binaryCachePublicKeys = [
      "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
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
