{ config, pkgs, lib, ... }:

with lib;

let

in {

  options.mine.taffybar.enable = mkEnableOption "taffybar config";

  config = mkIf config.mine.taffybar.enable {

    mine.xUserConfig = {

      systemd.user.services.taffybar = {
        Unit = {
          Description = "Taffybar";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${(import ./taffybar {}).mytaffybar}/bin/mytaffybar";
          Restart = "on-failure";
        };

        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };

    };

  };

}
