{ config, lib, pkgs, ...}:

with lib;

{
  options.mpd = {
    musicDir = mkOption {
      type = types.path;
      description = "Path to music directory";
    };

    port = mkOption {
      type = types.int;
      default = 6600;
      description = "Port for MPD API";
    };

    httpPort = mkOption {
      type = types.int;
      description = "Port for MPD HTTP stream";
    };

    bitRate = mkOption {
      type = types.int;
      default = 192;
      description = "MPD HTTP stream bitrate";
    };
  };

  config.mpd = {
    musicDir = "/home/infinisil/Music";
    httpPort = 8300;
  };
}
