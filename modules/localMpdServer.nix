{ config, lib, pkgs, ...}:

let

  cfg = config.mpd;

in

with lib;

{
  imports = [
    ./mpd.nix
    ./mpdClient.nix
  ];

  services.mpd = {
    enable = true;
    user = "infinisil";
    group = "users";
    musicDirectory = "${cfg.musicDir}/data";
    readonlyPlaylists = true;
    playlistDirectory = "${cfg.musicDir}/playlists";
    network.port = cfg.port;
    network.listenAddress = "0.0.0.0";
    extraConfig = ''
      replaygain "track"
      audio_output {
        type "pulse"
        name "MPD PulseAudio Output"
        server "127.0.0.1"
      }
      password "${config.private.passwords.mpd}@read,add,control"
    '';
  };

  hardware.pulseaudio.extraConfig = ''
    load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
  '';

  # Needs to be mounted before mpd is started and unmounted after mpd stops
  systemd.services.mpd.after = [ "home-infinisil-Music.mount" ];

}
