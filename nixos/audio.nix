{ config, lib, pkgs, ...}:

let
  music = "/home/infinisil/Music";
in
{

  hardware.bluetooth.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
      load-module module-switch-on-connect
    '';
  };

  environment.systemPackages = with pkgs; [
    blueman
    sonata
    beets
  ];

  environment.variables = {
    MPD_HOST = "127.0.0.1";
    MPD_PORT = "6600";
  };

  services.mpd = {
    enable = true;
    group = "audio";
    musicDirectory = "${music}/data";
    dataDir = "${music}/mpd";
    dbFile = "${music}/mpd/mpd.db";
    extraConfig = ''
      audio_output {
        type "pulse"
        name "MPD PulseAudio Output"
        server "127.0.0.1"
      }
    '';
  };

  systemd.services.mpdcurrentfifo = {
    enable = true;
    description = "Outputs the currently playing song into a fifo for xmobar to read";
    path = with pkgs; [ mpc_cli ];
    script = ''
      mpc current; mpc idleloop player | while read line; do mpc current; done >> ~/Music/mpd/current
    '';
    after = [ "mpd.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig.User = "infinisil";
    serviceConfig.Restart = "always";
  };

  users.extraUsers.infinisil.extraGroups = [ "audio" ];
}
