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
        options.outputTemplate = lib.mkOption {
          type = types.addCheck types.str (str: ! lib.hasPrefix "/" str);
          default = "%(upload_date)s %(title)s.%(ext)s";
          description = ''
            See <https://github.com/yt-dlp/yt-dlp#output-template>
          '';
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

    environment.autoUpdate.presets.yt-dlp = true;

    systemd.services = lib.mapAttrs' (name: value: lib.nameValuePair "youtube-playlist-download-${name}" {
      description = "Automatic youtube playlist download for ${name}";
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" "auto-update.service" ];

      preStart = "mkdir -p ${value.targetDir}";
      serviceConfig = {
        User = value.user;
        Group = value.group;
        ExecStart = lib.escapeShellArgs ([
          "${config.environment.autoUpdate.profile}/bin/youtube-dl"
          "--download-archive" "${value.targetDir}/.archive"
          "--add-metadata"
          "--ignore-errors"
          "--paths" value.targetDir
          "--output" value.outputTemplate
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
