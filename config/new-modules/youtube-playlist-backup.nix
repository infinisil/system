{ config, lib, pkgs, ... }:

let

  inherit (lib) types;
  cfg = config.services.youtube-playlist-download;

in

{

  options.services.youtube-playlist-download = {
    playlists = lib.mkOption {
      default = {};
      type = types.attrsOf (types.submodule {
        options.playlist = lib.mkOption {
          type = types.str;
        };
        options.targetDir = lib.mkOption {
          type = types.path;
        };
        options.user = lib.mkOption {
          type = types.str;
        };
        options.group = lib.mkOption {
          type = types.str;
        };
        options.period = lib.mkOption {
          type = types.str;
          default = "hourly";
        };
        options.extraOptions = lib.mkOption {
          type = types.listOf types.str;
          default = [];
        };
      });
    };
  };

  config = {

    environment.autoUpdate.presets.youtube-dl = true;

    systemd.services = lib.mapAttrs' (name: value: lib.nameValuePair "youtube-playlist-download-${name}" {
      description = "Automatic youtube playlist download for ${name}";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      preStart = "mkdir -p ${value.targetDir}";
      serviceConfig = {
        User = value.user;
        Group = value.group;
        ExecStart = lib.escapeShellArgs ([
          "${config.environment.autoUpdate.profile}/bin/youtube-dl"
          "--download-archive" "${value.targetDir}/.archive"
          "--add-metadata"
          "--ignore-errors"
          "--output" "${value.targetDir}/%(upload_date)s %(title)s.%(ext)s"
          value.playlist
        ] ++ value.extraOptions);
      };
    }) cfg.playlists;

    systemd.timers = lib.mapAttrs' (name: value: lib.nameValuePair "youtube-playlist-download-${name}" {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = value.period;
        Persistent = true;
      };
    }) cfg.playlists;

  };

}
