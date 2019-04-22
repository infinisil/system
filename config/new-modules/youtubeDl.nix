{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.mine.youtubeDl;

in

{

  options.mine.youtubeDl = {

    enable = mkEnableOption ''
      automatic YouTube download and beet import + play
    '';

    user = mkOption {
      type = types.str;
      description = "User to use for downloading and saving";
    };

    mpdHost = mkOption {
      type = types.str;
      description = "MPD host to use, consisting of <password>@<host>";
    };

    youtubePlaylist = mkOption {
      type = types.str;
      description = "YouTube playlist to use";
    };

    dataDir = mkOption {
      type = types.str;
      default = "$HOME/.local/share/youtube";
      description = "Path to use for saving downloaded videos and ids";
    };

  };

  config = mkIf cfg.enable {

    systemd.services.youtube = {
      description = "Automatic youtube download";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      preStart = ''
        mkdir -p ${cfg.dataDir}
      '';

      serviceConfig = {
        User = cfg.user;
        Environment = "MPD_HOST=${cfg.mpdHost}";
        Restart = "always";
        RestartSec = 10;
        ExecStart = let
          exec = pkgs.writeScript "exec" ''
            #!${pkgs.bash}/bin/bash
            echo Importing item
            ${pkgs.beets}/bin/beet import -sAI --set=now=1 "$1" 2>/dev/null
            echo Writing tags to file
            ${pkgs.beets}/bin/beet write now:1 2>/dev/null
            echo Waiting for mpd to finish updating
            sleep 1
            echo Playing now:1
            ${pkgs.beets}/bin/beet play -y now:1 2>/dev/null
          '';
          args = lib.concatStringsSep " " [
            "-x"
            "--download-archive ${cfg.dataDir}/ids"
            "--add-metadata"
            "--ignore-errors"
            "-o \"${cfg.dataDir}/videos/%(title)s.%(ext)s\""
            "--exec '${exec} {}'"
            cfg.youtubePlaylist
          ];
          youtube-script = pkgs.writeScript "youtube-script" ''
            #!${pkgs.bash}/bin/bash
            while true; do
              echo Checking for new videos
              ${pkgs.youtube-dl}/bin/youtube-dl ${args}
              echo Sleeping for 1 minute
              sleep 600
            done
          '';
        in youtube-script;
      };
    };

  };

}
