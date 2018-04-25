{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.server-sync;

in

{

  options.mine.server-sync = {

    enable = mkEnableOption "server sync";

    uploadDir = mkOption {
      type = types.str;
      example = "server/upload";
      description = "Directory to use for uploading";
    };

    dataDir = mkOption {
      type = types.str;
      example = "server/data";
      description = "Directory to use for all the data";
    };

    interval = mkOption {
      type = types.str;
      default = "hourly";
      example = "minutely";
      description = "Interval for syncing, see <literal>man systemd.time</literal>";
    };

    server = mkOption {
      type = types.str;
      example = "example.com";
      description = "Host to use for central image storage";
    };

  };

  config = mkIf cfg.enable {

    mine.userConfig = {

      systemd.user = {
        services.server-sync = {
          Unit = {
            Description = "Automatic server sync";
            After = [ "graphical-session-pre.target" ];
            PartOf = [ "graphical-session.target" ];
          };

          Service = {
            Type = "oneshot";
            ExecStart = "${pkgs.writeScriptBin "server-sync" ''
              #!${pkgs.stdenv.shell}
              set -euo pipefail
              export PATH="${lib.makeBinPath (with pkgs; [
                rsync
                openssh
              ])}:$PATH"
              dir="${cfg.uploadDir}"
              destdir="${cfg.dataDir}"

              echo "dir is $dir, destdir is $destdir"

              echo "Creating directories if they don't exist.."
              mkdir -p "$dir" "$destdir"
              ssh "${cfg.server}" mkdir -p "$destdir"

              echo "Uploading.."
              rsync -avz "$dir"/ "${cfg.server}:$destdir"
              rsync --remove-source-files -av "$dir/" "$destdir"

              echo "Syncing changes.."
              rsync -avz --delete "${cfg.server}:$destdir/" "$destdir"

              echo Done!
            ''}/bin/server-sync";
          };

          Install.WantedBy = [ "graphical-session.target" ];
        };
        timers.server-sync = {
          Unit.Description = "Automatic server sync timer";
          Timer.OnCalendar = cfg.interval;
          Install.WantedBy = [ "timers.target" ];
        };
      };
    };

  };

}
