{ options, config, lib, ... }:

with lib;

{

  time.timeZone = mkDefault "Europe/Zurich";

  mine.mainUsers = [ "root" ];

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

  nix.trustedUsers = [ "root" "@wheel" ];

  nixpkgs.config.allowUnfree = true;

  home-manager.useUserPackages = true;

  security.sudo.wheelNeedsPassword = false;

  boot.cleanTmpDir = true;

}
