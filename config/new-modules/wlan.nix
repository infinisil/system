{ config, pkgs, lib, ... }:

with lib;

{

  options.mine.wlan.enable = mkOption {
    type = types.bool;
    default = config.mine.hardware.wlan;
    description = "Enable wlan config";
  };

  config = mkIf config.mine.wlan.enable {
    environment.systemPackages = with pkgs; [
      wpa_supplicant_gui
    ];

    networking.wireless = {
      enable = true;
      userControlled.enable = true;
    };
  };
}
