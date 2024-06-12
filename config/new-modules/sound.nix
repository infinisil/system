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

    services.pipewire.wireplumber.configPackages = [
      (pkgs.writeTextDir "share/wireplumber/wireplumber.conf.d/51-spdif-noise.conf" ''
        monitor.alsa.rules = [
          {
            matches = [
              {
                device.profile.name = "iec958-stereo"
              }
            ]
            actions = {
              update-props = {
                dither.noise = 2,
                node.pause-on-idle = false,
                session.suspend-timeout-seconds = 0
              }
            }
          }
        ]
      '')
    ];

  };
}
