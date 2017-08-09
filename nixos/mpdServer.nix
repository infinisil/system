
{ config, lib, pkgs, ...}:

let
  music = "/home/infinisil/Music";
  mpdPort = 6600;
  mpdHttpPort = 8300;
  password = (import ./private.nix).mpdPassword;
in

{
  imports = [
    ./base.nix
  ];

  networking.firewall.allowedTCPPorts = [ mpdPort mpdHttpPort ];

  environment.variables = {
    MPD_HOST = "${password}@localhost";
    MPD_PORT = "${toString mpdPort}";
  };

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
	encoder		"lame"
        port            "${toString mpdHttpPort}"
        bitrate		"256"
        format          "44100:16:2"
	max_clients	"0"
      }
      password "${password}@read,add,control"
    '';
  };
}
