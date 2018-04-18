{ lib, config, pkgs, mylib, ... }:

with lib;

let

  dag = mylib.dag;

  cfg = config.mine.emacs;

  initEntry = types.submodule {
    options = {
      data = mkOption {
        type = types.lines;
        description = "emacs code to execute";
      };

      before = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "To be done before the given targets";
      };

      after = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "To be done after the given targets";
      };
    };
  };


  # TODO: Handle dag cycle error gracefully
  initFile = pkgs.writeText "init.el" (concatMapStringsSep "\n\n" ({ name, data }:
    "; Section ${name}\n${data}"
  ) (dag.dagTopoSort cfg.init).result);

  emacs = pkgs.emacsPackagesNg.emacsWithPackages (_: cfg.packages);

in

{

  options.mine.emacs = {
    enable = mkEnableOption "emacs config";

    package = mkOption {
      type = types.package;
      default = pkgs.emacs25;
      description = "Emacs package to use";
    };

    packages = mkOption {
      type = types.listOf types.package;
      default = [];
      description = "Emacs packages. Use pkgs.emacsPackagesNg to refer to them";
    };

    init = mkOption {
      type = with types; attrsOf (coercedTo str dag.entryAnywhere initEntry);
      default = dag.empty;
      description = "Init entries";
    };
  };

  config = mkIf cfg.enable {

    mine.emacs.init.pkgs = dag.entryAnywhere ''
      (package-initialize)
    '';

    mine.userConfig = {

      home.file.".emacs.d/init.el".source = initFile;

      home.packages = [
        emacs
      ];

      systemd.user.services.emacs = {
        Unit = {
          Description = "Emacs editor";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Install.WantedBy = [ "default.target" ];

        Service = {
          Type = "forking";
          ExecStart = "${emacs}/bin/emacs --daemon";
          ExecStop = "${emacs}/bin/emacsclient --eval (kill-emacs)";
          Restart = "always";
        };
      };

    };
  };

}
