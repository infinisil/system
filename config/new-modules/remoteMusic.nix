{ lib, config, pkgs, ... }:
let
  inherit (lib) types;
  cfg = config.mine.remoteMusic;
  musicDir = "/var/lib/music";
  opusPort = 6726;
  mp3Port = 6725;
in {
  options.mine.remoteMusic = {
    enable = lib.mkEnableOption "remoteMusic";
  };

  config = lib.mkIf cfg.enable {

    services.mpd = {
      enable = true;
      credentials = [
        {
          passwordFile = config.secrets.files.mpd.file;
          permissions = [ "read" "add" "control" "admin" ];
        }
      ];
      musicDirectory = musicDir + "/data";
      playlistDirectory = musicDir + "/playlists";
      network.listenAddress = "any";
      extraConfig = ''
        audio_output {
          type            "httpd"
          name            "My HTTP Stream, opus"
          encoder         "opus"
          signal          "music"
          complexity      "10"
          port            "${toString opusPort}"
          bitrate         "128000"
          format          "48000:16:2"
          max_clients     "0"
          always_on       "yes"
        }
        audio_output {
          type            "httpd"
          name            "My HTTP Stream, normal mp3 lame"
          encoder         "lame"
          port            "${toString mp3Port}"
          bitrate         "192"
          format          "44100:16:2"
          max_clients     "0"
          always_on "yes"
        }
      '';
    };

    networking.firewall.allowedTCPPorts = [ config.services.mpd.network.port opusPort mp3Port ];

    services.nginx = {
      enable = true;
      virtualHosts."tune.infinisil.com" = {
        root = "/webroot";
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:${toString mp3Port}";
        locations."/opus/".proxyPass = "http://localhost:${toString opusPort}";
      };
    };

  };
}
