
{ config, lib, pkgs, ...}:

with lib;

let cfg = config.mpd; in {
  imports = [
    ./mpd.nix
    ./mpdClient.nix
  ];

  networking.firewall.allowedTCPPorts = [ cfg.port cfg.httpPort ];

  services.mpd = let cfg = config.mpd; in {
    enable = true;
    user = "infinisil";
    group = "users";
    musicDirectory = "${cfg.musicDir}/data";
    dataDir = "${cfg.musicDir}/mpd";
    dbFile = "${cfg.musicDir}/mpd/mpd.db";
    network.listenAddress = "0.0.0.0";
    network.port = cfg.port;
    extraConfig = ''
      audio_output {
        type            "httpd"
        name            "My HTTP Stream"
        encoder         "lame"
        port            "${toString cfg.httpPort}"
        bitrate         "${toString cfg.bitRate}"
        format          "44100:24:2"
        max_clients     "0"
        mixer_type      "software"
      }
      password "${confg.passwords.mpd}@read,add,control"
    '';
  };

  # Needs to be mounted before mpd is started and unmounted after mpd stops
  systemd.services.mpd.serviceConfig.after = [ "home-infinisil-Music.mount" ];
}
