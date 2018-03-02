
{ config, lib, pkgs, ...}:

with lib;

let cfg = config.mpd; in {
  imports = [
    ./mpd.nix
    ./mpdClient.nix
  ];

  networking.firewall.allowedTCPPorts = optionals (! cfg.local) [ cfg.port 6725 ];

  services.mpd = {
    enable = true;
    user = "infinisil";
    group = "users";
    musicDirectory = "${cfg.musicDir}/data";
    readonlyPlaylists = ! cfg.local;
    playlistDirectory = "${cfg.musicDir}/playlists";
    network.port = cfg.port;
    network.listenAddress = "0.0.0.0";
    extraConfig = if cfg.local then ''
      audio_output {
        type "pulse"
        name "MPD PulseAudio Output"
        server "127.0.0.1"
      }
    '' else ''
      audio_output {
        type            "httpd"
        name            "My HTTP Stream, opus"
        encoder         "opus"
        signal          "music"
        complexity      "10"
        port            "6725"
        bitrate         "128000"
        format          "48000:16:2"
        max_clients     "0"
        always_on       "yes"
      }
      audio_output {
        type            "httpd"
        name            "My HTTP Stream, normal mp3 lame"
        encoder         "lame"
        port            "${toString cfg.httpPort}"
        bitrate         "${toString cfg.bitRate}"
        format          "44100:16:2"
        max_clients     "0"
        always_on "yes"
      }
      password "${config.private.passwords.mpd}@read,add,control"
    '' + ''
      replaygain "track"
    '';
  };

  mine.subdomains = [ "tune" ];

  services.nginx.virtualHosts."tune.${config.networking.domain}" = {
    root = "/webroot";
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://localhost:${toString cfg.httpPort}";
    locations."/opus/".proxyPass = "http://localhost:6725";
    #basicAuth.infinisil = config.private.passwords."tune.infinisil.com";
  };
  hardware.pulseaudio.${if cfg.local then "extraConfig" else null} = ''
    load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
  '';

  # Needs to be mounted before mpd is started and unmounted after mpd stops
  systemd.services.mpd.after = [ "home-infinisil-Music.mount" ];
}
