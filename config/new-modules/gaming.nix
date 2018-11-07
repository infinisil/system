{ lib, config, pkgs, ... }:

with lib;

{

  options.mine.gaming.enable = mkEnableOption "games";

  config = mkIf config.mine.gaming.enable {

    hardware.steam-hardware.enable = true;
    boot.blacklistedKernelModules = [ "hid_steam" ];

    environment.systemPackages = with pkgs; [
      steam
      minecraft
      #(wineStaging.override {
      #  wineBuild = "wineWow";
      #  gstreamerSupport = false;
      #})
    ];

  };

}
