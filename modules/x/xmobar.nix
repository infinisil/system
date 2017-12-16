{ config, pkgs, lib, ... }:

with lib;
with import ../../scripts { inherit pkgs lib; };

let
  home = config.home-manager.users.infinisil;

  configFile = let
    fonts = lib.concatStringsSep "," [
      "Helvetica Neue LT Std,HelveticaNeueLT Std Lt Cn:style=47 Light Condensed,Regular:pixelsize=12"
      "FantasqueSansMono Nerd Font:pixelsize=12"
      "Noto Sans CJK JP,Noto Sans CJK JP Thin:style=Thin,Regular"
      "M+ 2p,M+ 2p light:style=light,Regular"
      "Noto Emoji:style=Regular:pixelsize=10"
    ];
  in pkgs.writeText "xmobar-config" ''
    Config
      { font = "xft:${fonts}"
      , bgColor = "#2b2b29"
      , fgColor = "#c3ae93"
      , alpha = 210
      , position = Top
      , commands =
        [	Run Cpu
          [ "-t", "<total>%"
          , "-L", "10"
          , "-H", "50"
          , "-l", "green"
          , "-h", "red" ] 10
        , Run CoreTemp
          [ "-t", "<core0>/<core1>°C "
          , "-L", "65"
          ,	"-H", "90"
          ,	"-l", "lightblue"
          ,	"-h", "red" ] 50
        , Run Date "%a %d.%m %T" "date" 10
        , Run XMonadLog
        , Run DynNetwork
          [ "-t" , "<tx><rx> | "
          , "-L" , "10000"
          , "-H" , "500000"
          , "-l" , "green"
          , "-n" , "orange"
          , "-h" , "red" ] 10
        , Run CommandReader "${pkgs.writeScript "info" ''
            #!${pkgs.bash}/bin/bash
            export MPD_HOST="${home.home.sessionVariables.MPD_HOST}"
            ${pkgs.mpc_cli}/bin/mpc waitmessage info &
            ${pkgs.mpc_cli}/bin/mpc sendmessage updateInfo update
            while true; do
              ${pkgs.mpc_cli}/bin/mpc waitmessage info
              if [ $? != 0 ]; then
                sleep 1
              fi
            done
          ''}" "info"
        , Run Com "${power}" [] "power" 10
        , Run Com "${batt}" [] "bt" 50
        , Run Com "${playing}" [] "playing" 10
      ]
      , sepChar = "%"
      , alignSep = "}{"
      , template = "%XMonadLog% } %info%  %playing% { %power%A  | %dynnetwork%%cpu%  | %bt% | <fc=#ee9a00>%date%</fc>"
      }
  '';
in {

  home-manager.users.infinisil = {

    systemd.user.services.xmobar = {
      Unit = {
        Description = "Xmobar";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${pkgs.haskellPackages.xmobar}/bin/xmobar ${configFile}";
        Restart = "on-failure";
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };

}
