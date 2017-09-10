
{ config, lib, pkgs, ...}:

let
  password = (import ./private.nix).mpd;
in

with lib;

{
  imports = [
    ./base.nix
    ./mpd.nix
    ./mpdClient.nix
  ];

  networking.firewall.allowedTCPPorts = [ config.mpd.port config.mpd.httpPort ];

  services.mpd = {
    enable = true;
    user = "infinisil";
    group = "users";
    musicDirectory = "${music}/data";
    dataDir = "${music}/mpd";
    dbFile = "${music}/mpd/mpd.db";
    network.listenAddress = "0.0.0.0";
    network.port = mpdPort;
    extraConfig = ''
      audio_output {
        type            "httpd"
        name            "My HTTP Stream"
        encoder		      "lame"
        port            "${toString config.mpd.httpPort}"
        bitrate		      "${toString config.mpd.bitRate}"
        format          "44100:24:2"
        max_clients	    "0"
        mixer_type	    "software"
      }
      password "${password}@read,add,control"
    '';
  };

  # Needs to be mounted before mpd is started and unmounted after mpd stops
  systemd.services.mpd.serviceConfig.after = [ "home-infinisil-Music.mount" ];
}
