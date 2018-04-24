{ lib, config, pkgs, ... }:

with lib;

let

  cfg = config.mine.emacs;

in

{

  options.mine.emacs = {
    enable = mkEnableOption "emacs config";

    config = mkOption {
      type = types.submodule (import ./submodule.nix);
      default = {};
      description = "submodule";
    };
  };

  config = mkIf cfg.enable {

    mine.userConfig = {

      home.file.".emacs.d/init.el".source = cfg.config.initFile;

      home.packages = [ cfg.config.emacs ];

      systemd.user.services.emacs = {
        Unit = {
          Description = "Emacs editor";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Install.WantedBy = [ "default.target" ];

        Service = {
          Type = "forking";
          ExecStart = "${cfg.config.emacs}/bin/emacs --daemon";
          ExecStop = "${cfg.config.emacs}/bin/emacsclient --eval (kill-emacs)";
          Restart = "always";
        };
      };

    };
  };

}
