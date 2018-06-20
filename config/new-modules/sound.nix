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
      mine.say
      (mkIf config.hardware.bluetooth.enable blueman)
      pavucontrol
      cli-visualizer
    ];

    mine.userConfig = {
      home.file.".config/vis/config".text = ''
        audio.sources=pulse
      '';
    };

    hardware.pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      extraConfig = ''
        load-module module-switch-on-connect
      '';
    };

  };
}
