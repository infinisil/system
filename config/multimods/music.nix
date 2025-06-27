{ lib, config, ... }:
let
  inherit (lib) types;
  cfg = config.music;
  mpdHost = config.nodes.${cfg.server.node}.configuration.networking.domain;
  mpdPort = config.nodes.${cfg.server.node}.configuration.services.mpd.network.port;

  sharedConfig = { config, pkgs, ... }: {
    options.users.users = lib.mkOption {
      type = types.attrsOf (types.submodule ({ config, ... }: {
        config = lib.mkIf config.isNormalUser {
          extraGroups = [ "music" ];
        };
      }));
    };
    config = {
      secrets.files.mpd = {
        file = cfg.passwordFile;
        group = "music";
      };
      environment.systemPackages = [ pkgs.mpc ];

      users.groups.music = {};
    };

  };

  serverConfig = { pkgs, config, ... }: {

    users.users.music.isNormalUser = true;
    users.users.music.group = "music";

    services.mpd = {
      enable = true;
      user = "music";
      group = "music";
      credentials = [
        {
          passwordFile = config.secrets.files.mpd.file;
          permissions = [ "read" "add" "control" /* "player" */ "admin" ];
        }
      ];
      musicDirectory = "${cfg.server.musicDir}/data";
      playlistDirectory = "${cfg.server.musicDir}/playlists";
      network.listenAddress = "any";
      extraConfig = ''
        local_permissions "read,add,control,player,admin"

        audio_output {
          type            "httpd"
          name            "My HTTP Stream, opus"
          encoder         "opus"
          signal          "music"
          complexity      "10"
          port            "${toString cfg.server.opusPort}"
          bitrate         "128000"
          format          "48000:16:2"
          max_clients     "0"
          always_on       "yes"
        }
        audio_output {
          type            "httpd"
          name            "My HTTP Stream, normal mp3 lame"
          encoder         "lame"
          port            "${toString cfg.server.mp3Port}"
          bitrate         "192"
          format          "44100:16:2"
          max_clients     "0"
          always_on "yes"
        }

        include_optional "/etc/mpd-dynamic.conf"
      '';
    };

    services.nginx = {
      enable = true;
      virtualHosts.${cfg.server.domain} = {
        enableACME = true;
        forceSSL = true;
        locations."/mp3/".proxyPass = "http://localhost:${toString cfg.server.mp3Port}";
        locations."/opus/".proxyPass = "http://localhost:${toString cfg.server.opusPort}";
      };
    };

    services.samba = {
      enable = true;
      openFirewall = true;
      shares.music = {
        path = "/var/lib/music/data";
      };
    };

    networking.firewall.allowedTCPPorts = [
      80
      443
      cfg.server.mp3Port
      cfg.server.opusPort
      config.services.mpd.network.port
    ];

    home-manager.users.music = {
      programs.beets.enable = true;
      programs.beets.settings = {
        directory = "${cfg.server.musicDir}/data";
        library = "${cfg.server.musicDir}/beets/beets.db";
        # Allows specifying smart playlists without having to rebuild the system
        include = [ "${cfg.server.musicDir}/beets/dynamic.yaml" ];
        format_item = "$id: $artist - $album - $title - $rating";
        sort_item = "added- artist+ album+ disc+ track+";
        acoustid.apikey = "ex5RgecjNm";

        import = {
          incremental = true;
          log = "${cfg.server.musicDir}/beets/import.log";
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
            #systemctl --user stop mpdstats.service
            mpc clear
            mpc add < "$1"
            mpc play
            #systemctl --user start mpdstats.service
          '';
          relative_to = "${cfg.server.musicDir}/data";
          warning_threshold = false;
        };

        replaygain = {
          backend = "ffmpeg";
          overwrite = true;
        };

        smartplaylist = {
          playlist_dir = "${cfg.server.musicDir}/playlists";
          relative_to = "${cfg.server.musicDir}/data";
        };
      };
    };
  };

  clientConfig = { config, pkgs, ... }: {
    environment.interactiveShellInit = ''
      MPD_HOST=$(<${config.secrets.files.mpd.file})@${mpdHost}
      MPD_PORT=${toString mpdPort}
    '';
  };
in
{
  options.music = {
    server.node = lib.mkOption {
      type = types.str;
    };

    passwordFile = lib.mkOption {
      type = types.path;
    };

    server.musicDir = lib.mkOption {
      type = types.path;
    };

    server.opusPort = lib.mkOption {
      type = types.port;
      default = 6742;
    };

    server.mp3Port = lib.mkOption {
      type = types.port;
      default = 6743;
    };

    server.domain = lib.mkOption {
      type = types.str;
    };

    client.nodes = lib.mkOption {
      type = types.listOf types.str;
    };

  };

  config = {
    nodes = lib.mkMerge [
      { ${cfg.server.node}.configuration = lib.mkMerge [ serverConfig sharedConfig ]; }
      (lib.genAttrs cfg.client.nodes (node: {
        configuration = lib.mkMerge [ clientConfig sharedConfig ];
      }))
    ];
  };
}
