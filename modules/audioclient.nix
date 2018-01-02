{ config, pkgs, lib, ... }:

with lib;

let

  beet = pkgs.writeScriptBin "beet" ''
    #!${pkgs.bash}/bin/bash
    ssh -q -t inf "beet ''${@@Q} 2>/dev/null"
  '';

  home = config.home-manager.users.infinisil;

  musicDir = "${home.homeDirectory}/Music";

in

{

  home-manager.users.infinisil = {

    home.packages = [
      beet
    ];

    systemd.user.services.music = {
      Unit = {
        Description = "Play music";
        After = [ "graphical-session-pre.target" "network.target" ];
      };

      Service = {
        ExecStart = "${pkgs.mpv}/bin/mpv http://tune.infinisil.com/opus --quiet";
        Restart = "on-success";
        SuccessExitStatus = 4;
        RestartPreventExitStatus = 4;
      };
    };

  };

}

