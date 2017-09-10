{ config, lib, pkgs, ...}:

let
  music = "/home/infinisil/Music";
  mpdHttpPort = 8300;
  password = (import ./private.nix).mpd;
in

{
  imports = [
    ./base.nix
  ];

  hardware.bluetooth.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
      load-module module-switch-on-connect
    '';
  };

  #users.extraUsers.infinisil.extraGroups = [ "audio" ];

  #users.extraGroups.audio = {};

  environment.systemPackages = with pkgs; [
    blueman
    sonata
    beets
    pavucontrol
  ];

  environment.variables = {
    MPD_HOST = "${password}@infinisil.io";
    MPD_PORT = "6600";
  };

  #networking.firewall.allowedTCPPorts = [ 6600 mpdHttpPort ];

  services.mpd = {
    #enable = true;
    group = "audio";
    musicDirectory = "${music}/data";
    dataDir = "${music}/mpd";
    dbFile = "${music}/mpd/mpd.db";
    network.listenAddress = "0.0.0.0";
    extraConfig = ''
      audio_output {
        type "pulse"
        name "MPD PulseAudio Output"
        server "127.0.0.1"
      }
      audio_output {
        type            "httpd"
        name            "My HTTP Stream"
        port            "${toString mpdHttpPort}"
        quality         "5.0"
        format          "44100:16:1"
      }
    '';
  };

  #systemd.services.mpdcurrentfifo = {
  #  enable = true;
  #  description = "Outputs the currently playing song into a fifo for xmobar to read";
  #  path = with pkgs; [ mpc_cli ];
  #  script = ''
  #    mpc current; mpc idleloop player | while read line; do mpc current; done >> ~/Music/mpd/current
  #  '';
  #  after = [ "mpd.service" ];
  #  wantedBy = [ "multi-user.target" ];
  #  serviceConfig.User = "infinisil";
  #  serviceConfig.Restart = "always";
  #};
}
