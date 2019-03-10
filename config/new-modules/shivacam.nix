{ lib, pkgs, config, ... }:

with lib;

let

  cfg = config.shivacam;

in {

  options.shivacam = {
    cam.enable = mkEnableOption "Shivacam camera";
    viewer.host = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Host to use for shivacam.";
    };
  };

  config = mkMerge [
    (mkIf cfg.cam.enable {
      systemd.sockets.shivacam = {
        wantedBy = [ "sockets.target" ];
        socketConfig = {
          RuntimeDirectory = "shivacam";
          ListenStream = "/run/shivacam/socket";
          Accept = true;
          # /dev/video devices can't be read by more than one process at once
          MaxConnections = 1;
        };
      };

      environment.systemPackages = [
        (pkgs.writeScriptBin "shivastream" ''
          #!${pkgs.stdenv.shell}
          exec ${pkgs.nmap}/bin/ncat -U /run/shivacam/socket "$@"
        '')
      ];

      systemd.services."shivacam@" = {
        script = ''
          ${pkgs.ffmpeg-full}/bin/ffmpeg \
            -f v4l2 \
            -framerate 30 \
            -video_size 640x480 \
            -i /dev/video0 \
            -f avi \
            - \
            2>/dev/null
        '';
        serviceConfig.StandardOutput = "socket";
      };
    })
    (mkIf (cfg.viewer.host != null) {
      environment.systemPackages = [
        (pkgs.writeScriptBin "shivacam" ''
          #!${pkgs.stdenv.shell}
          ssh "${config.networking.connections.${cfg.viewer.host}}" shivastream \
            | mpv - \
              --demuxer-thread=no \
              --cache=no \
              --title=shivacam \
              --geometry=15%+2155+103 \
              "$@"
        '')
      ];
    })
  ];

}
