{ config, lib, ... }:

with lib;

{

  options.mine.music = {

    server = {

      enable = mkEnableOption "MPD server config";

      local = mkOption {
        type = types.bool;
        description = "Whether this should be a local MPD server with pulseaudio";
      };

      musicDir = mkOption {
        type = types.path;
        description = "Path to the music directory";
      };

      user = mkOption {
        type = types.str;
        description = "user to run mpd as";
      };

      group = mkOption {
        type = types.str;
        description = "group to run mpd as";
      };

      password = mkOption {
        type = types.str;
        description = "MPD password";
      };

      port = mkOption {
        type = types.ints.u16;
        default = 6600;
        description = "Port for MPD";
      };

      mp3Port = mkOption {
        type = types.ints.u16;
        default = 6725;
        description = "Port for MP3 webstream";
      };

      opusPort = mkOption {
        type = types.ints.u16;
        default = 6726;
        description = "Port for opus webstream";
      };

    };

    client = {

      enable = mkOption {
        type = types.bool;
        default = config.mine.music.server.enable;
        description = "enable music client config";
      };

      listen = mkOption {
        type = types.bool;
        default = config.mine.hardware.audio;
        description = "enable music listening";
      };

      server = mkOption {
        type = types.nullOr types.unspecified;
        default = null;
        description = "Server to use, null for own machine";
      };

    };
  };

}
