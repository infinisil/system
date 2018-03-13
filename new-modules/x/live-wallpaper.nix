{ config, pkgs, lib, ... }:

with lib;

{

  options.mine.live-wallpaper.enable = mkEnableOption "live-wallpaper config";

  config.mine.xUserConfig = mkIf config.mine.live-wallpaper.enable {

    systemd.user.services.live-wallpaper = {

      Unit.Description = "Animated wallpaper";

      Service = {
        ExecStart = let
          args = lib.concatStringsSep " " [
            "-ov"
            "-fs"
            "-ni"
            "--"
            "${pkgs.mpv}/bin/mpv"
            "--loop=inf"
            "-wid WID"
            "--panscan=1"
            "--quiet"
            "/home/infinisil/pics/live/fav/P3v8uQ2.mp4"
          ];
        in "${pkgs.xwinwrap}/bin/xwinwrap ${args}";
      };
    };
  };
}
