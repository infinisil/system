{ pkgs, lib, config, ... }:

with lib;

let

  cfg = config.mine.compton;

  package = pkgs.mine.compton-kawase;

  mkComptonService = { autoStart, variantName, cfg }: recursiveUpdate {
    Unit.Description = "Compton X11 compositor - ${variantName}";

    Service = {
      ExecStart = "${package}/bin/compton --config ${cfg}";
      Restart = "always";
      RestartSec = 3;
    };

    Install.WantedBy = [ "graphical-session.target" ];

  } (optionalAttrs autoStart {
    Unit = {
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

  });

in

{
  options.mine.compton = {

    enable = mkEnableOption "compton config";

    highend = mkEnableOption "use high-end by default";

  };

  config = mkIf cfg.enable {

    mine.xUserConfig = {

      home.packages = [ package ];

      systemd.user.services = {
        compton-high = recursiveUpdate (mkComptonService {
          variantName = "Highend";
          cfg = ./compton-high.conf;
          autoStart = cfg.highend;
        }) {
          Unit.Conflicts = [ "compton-low.service" ];
        };

        compton-low = recursiveUpdate (mkComptonService {
          variantName = "Lowend";
          cfg = ./compton-low.conf;
          autoStart = ! cfg.highend;
        }) {
          Unit.Conflicts = [ "compton-high.service" ];
        };
      };
    };
  };
}
