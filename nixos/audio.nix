{ config, lib, pkgs, ...}:

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
    BEETSDIR = "/music/beets";
    MPD_HOST = "127.0.0.1";
    MPD_PORT = "6600";
  };

  services.mpd = {
    enable = true;
    group = "audio";
    musicDirectory = "/music/data";
    dataDir = "/music/mpd";
    dbFile = "/music/mpd/mpd.db";
    extraConfig = ''
      audio_output {
        type "pulse"
        name "MPD PulseAudio Output"
        server "127.0.0.1"
      }
    '';
  };

  users.extraUsers.infinisil.extraGroups = [ "audio" ];
}
