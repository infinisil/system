{ lib, config, pkgs, ... }:
let
  inherit (lib) types;
  cfg = config.mine.localMusic;
  musicDir = "/home/infinisil/music";
  streamPort = 6601;
in {
  options.mine.localMusic = {
    enable = lib.mkEnableOption "localMusic";
    musicDir = lib.mkOption {
      type = types.str;
      default = "/home/infinisil/music";
    };
  };

  config = lib.mkIf cfg.enable {

    networking.firewall.allowedTCPPorts = [ 6600 streamPort ];

    home-manager.users.infinisil = { config, ... }: {

      services.mpd = {
        enable = true;
        musicDirectory = "${musicDir}/data";
        playlistDirectory = "${musicDir}/playlists";
        network.listenAddress = "any";
        extraConfig = ''
          audio_output {
              type "pipewire"
              name "Pipewire"
          }
          audio_output {
              type            "httpd"
              name            "My HTTP Stream, normal mp3 lame"
              encoder         "lame"
              port            "${toString streamPort}"
              bitrate         "192"
              format          "44100:16:2"
              max_clients     "0"
              always_on       "yes"
          }

          # Two beets bugs:
          # - It doesn't run replaygain automatically (even though it's configured) when importing tracks
          # - It writes r128_album_gain tags to opus files, but with value 0, which the player then uses, and is usually way too loud
          # This code only fixes the second one. The first one is just written down here because I'm too lazy to put it somewhere else
          replaygain "track"

          password "${lib.fileContents ../../external/private/mpd}@read,add,control,admin,player"
          host_permissions "127.0.0.1 read,add,control,player,admin"
        '';
      };

      systemd.user.services.musicInfo = let
        musicInfo = pkgs.writers.writePython3 "musicInfo" {
          libraries = [ pkgs.python3.pkgs.mpd2 ];
          flakeIgnore = [ "E501" ];
        } ./musicInfo.py;
      in {
        Unit = {
          Requires = [ "mpd.service" ];
          After = [ "mpd.service" ];
        };

        Install.WantedBy = [ "default.target" ];

        Service = {
          ExecStart = "${musicInfo}";
          Restart = "on-failure";
        };
      };

      systemd.user.services.mpdstats = {
        Unit = {
          Requires = [ "mpd.service" ];
          After = [ "mpd.service" ];
        };

        Install.WantedBy = [ "default.target" ];

        Service = {
          ExecStart = "${lib.getBin config.programs.beets.package}/bin/beet mpdstats";
          Restart = "on-failure";
        };
      };

      home.packages = with pkgs; [
        mpc
        ncmpcpp
      ];

      systemd.user.services.mpd.Service.Restart = "on-failure";

      programs.zsh.shellAliases.beet = "noglob beet";

      programs.beets.enable = true;
      programs.beets.settings = {
        directory = "${musicDir}/data";
        library = "${musicDir}/beets/beets.db";
        # Allows specifying smart playlists without having to rebuild the system
        include = [ "${musicDir}/beets/dynamic.yaml" ];
        format_item = "$id: $artist - $album - $title - $rating";
        sort_item = "added- artist+ album+ disc+ track+";
        acoustid.apikey = "ex5RgecjNm";

        import = {
          incremental = true;
          log = "${musicDir}/beets/import.log";
        };

        plugins = [
          "smartplaylist"
          "fromfilename"
          "edit"
          "fetchart"
          "mpdstats"
          "mpdupdate"
          "ftintitle"
          "replaygain"
          "play"
          "types"
          "chroma"
        ];

        types = {
          star_rating = "int";
        };

        play = {
          command = pkgs.writeShellScript "play" ''
            systemctl --user stop mpdstats.service
            mpc clear
            mpc add < "$1"
            mpc play
            systemctl --user start mpdstats.service
          '';
          relative_to = "${musicDir}/data";
          warning_threshold = false;
        };

        replaygain = {
          backend = "ffmpeg";
          overwrite = true;
        };

        smartplaylist = {
          playlist_dir = "${musicDir}/playlists";
          relative_to = "${musicDir}/data";
        };
      };
    };
  };
}
