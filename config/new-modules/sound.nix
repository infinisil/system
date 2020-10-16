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
      pulseeffects
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
        unload-module module-switch-on-port-available
      '';
    };

    mine.userConfig.systemd.user.services.pulseeffects = {
      Unit = {
        After = [ "graphical-session-pre.target" ];
        Requires = [ "pulseaudio.service" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${pkgs.pulseeffects}/bin/pulseeffects --gapplication-service";
        # https://github.com/wwmm/pulseeffects/issues/533
        Environment = "PATH=${pkgs.pulseaudio}/bin";
      };

      Install.WantedBy = [ "graphical-session.target" ];
    };

  };
}
