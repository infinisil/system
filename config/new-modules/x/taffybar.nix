{ config, pkgs, lib, ... }:

with lib;

let

in {

  options.mine.taffybar.enable = mkEnableOption "taffybar config";

  config = mkIf config.mine.taffybar.enable {

    services.upower.enable = true;

    services.dbus.packages = [ pkgs.mpdris2 ];

    mine.xUserConfig = {

      systemd.user.services.mpDris2 = {
        Unit.Description = "mpDris2 - Music Player Daemon D-Bus bridge";
        Service.ExecStart = "${pkgs.mpdris2}/bin/mpDris2";
        Service.BusName = "org.mpris.MediaPlayer2.mpd";
        Service.Environment = "MPD_HOST=${config.mine.mpdHost} MPD_PORT=${config.mine.mpdPort}";
        Install.WantedBy = [ "default.target" ];
      };

      systemd.user.services.taffybar = {
        Unit = {
          Description = "Taffybar";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${(import ./taffybar {}).mytaffybar}/bin/mytaffybar";
          Restart = "on-failure";
        };

        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };

    };

  };

}
