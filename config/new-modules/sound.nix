{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.sound = {
    enable = mkOption {
      type = types.bool;
      default = config.mine.hardware.audio;
      description = "enable audio config";
    };
  };

  config = mkIf config.mine.sound.enable {

    environment.systemPackages = with pkgs; [
      #mine.say
      #(mkIf config.hardware.bluetooth.enable blueman)
      pavucontrol
      easyeffects
      qjackctl
    ];

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };

  };
}
