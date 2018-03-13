{ config, lib, ... }:

{

  time.timeZone = lib.mkDefault "Europe/Zurich";

  mine.mainUsers = [ "infinisil" "root" ];

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
