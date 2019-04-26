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
    trustedUsers = [ "root" "@wheel" ];
    daemonNiceLevel = 1;
  };

  nixpkgs = {
    config.allowUnfree = true;

    overlays = [
      (import ../../../external/home-manager/overlay.nix)
    ];
  };

  home-manager.useUserPackages = true;

  security.sudo.wheelNeedsPassword = false;

  boot.cleanTmpDir = true;

}
