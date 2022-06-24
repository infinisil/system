{ lib, config, pkgs, ... }:
let
  inherit (lib) types;
  cfg = config.mine.localMusic;
  musicDir = "/home/infinisil/music";
in {
  options.mine.localMusic = {
    enable = lib.mkEnableOption "localMusic";
  };

  config = lib.mkIf cfg.enable {

    home-manager.users.infinisil = {

      services.mpd = {
        enable = true;
        musicDirectory = "${musicDir}/data";
        playlistDirectory = "${musicDir}/playlists";
        extraConfig = ''
          audio_output {
              type "pipewire"
              name "Pipewire"
          }

          replaygain "auto"
        '';
      };

      systemd.user.services.musicInfo = let
        musicInfo = pkgs.writers.writePython3 "musicInfo" {
          libraries = [ pkgs.python3.pkgs.mpd2 ];
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

      home.packages = with pkgs; [
        mpc_cli
        ncmpcpp
      ];

      programs.zsh.shellAliases.beet = "noglob beet";

      programs.beets.settings = {
        directory = "${musicDir}/data";
        library = "${musicDir}/beets/beets.db";
        # Allows specifying smart playlists without having to rebuild the system
        include = [ "${musicDir}/beets/dynamic.yaml" ];
        format_item = "$id: $artist - $album - $title - $rating";
        sort_item = "added- artist+ album+ disc+ track+";
        acoustid.apikey = "ex5RgecjNm";

        import = {
          move = true;
          resume = true;
          incremental = true;
          log = "${musicDir}/beets/import.log";
        };

        plugins = [
          "smartplaylist"
          "fromfilename"
          "edit"
          "fetchart"
          "mpdupdate"
          "ftintitle"
          "replaygain"
          "play"
          "types"
          "chroma"
        ];

        types = {
          rating = "float";
        };

        play = {
          command = pkgs.writeShellScript "play" ''
            mpc clear
            mpc add < "$1"
            mpc play
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
