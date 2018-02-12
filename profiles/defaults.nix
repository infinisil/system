{ pkgs, ... }:

{

  imports = [
    ../modules/ch.nix
    ../modules/keys.nix
    ../modules/sshclient.nix
    ../lib
    ../private
    ../home-manager/nixos
  ];

  mine.mainUsers = [ "infinisil" "root" ];

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

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ../home-manager/overlay.nix)
  ];

  security.sudo.wheelNeedsPassword = false;

  boot.loader.grub.splashImage = null;

  boot.cleanTmpDir = true;
}
