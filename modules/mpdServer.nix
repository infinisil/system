
{ config, lib, pkgs, ...}:

with lib;

let cfg = config.mpd; in {
  imports = [
    ./mpd.nix
    ./mpdClient.nix
  ];

  networking.firewall.allowedTCPPorts = optionals (! cfg.local) [ cfg.port ];

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
        name            "My HTTP Stream"
        encoder         "lame"
        port            "${toString cfg.httpPort}"
        bitrate         "${toString cfg.bitRate}"
        format          "44100:16:2"
        max_clients     "0"
        always_on "yes"
      }
      password "${config.passwords.mpd}@read,add,control"
    '' + ''
      replaygain "track"
    '';
  };
  services.nginx.virtualHosts."tune.${config.networking.domain}" = {
    forceSSL = true;
    enableACME = true;
    root = "/webroot";
    locations."/".proxyPass = "http://localhost:${toString cfg.httpPort}";
  };
  hardware.pulseaudio.${if cfg.local then "extraConfig" else null} = ''
    load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
  '';

  # Needs to be mounted before mpd is started and unmounted after mpd stops
  systemd.services.mpd.serviceConfig.after = [ "home-infinisil-Music.mount" ];
}
