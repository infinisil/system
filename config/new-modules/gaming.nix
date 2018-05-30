{ lib, config, pkgs, ... }:

with lib;

{

  options.mine.gaming.enable = mkEnableOption "games";

  config = mkIf config.mine.gaming.enable {

    environment.systemPackages = with pkgs; [
      steam
      minecraft
      (wine.override { wineBuild = "wineWow"; })
    ];

  };

}
