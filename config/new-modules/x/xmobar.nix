{ config, pkgs, lib, ... }:

with lib;

let

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
        , Run Memory [] 10
        , Run DynNetwork
          [ "-t" , "<tx><rx> KB/s | "
          , "-L" , "10000"
          , "-H" , "500000"
          , "-l" , "green"
          , "-n" , "orange"
          , "-h" , "red" ] 10
        , Run CommandReader "${pkgs.writeScript "info" ''
            #!${pkgs.bash}/bin/bash
            export MPD_HOST="${config.mine.mpdHost}"
            ${pkgs.mpc_cli}/bin/mpc waitmessage info &
            ${pkgs.mpc_cli}/bin/mpc sendmessage updateInfo update
            while true; do
              ${pkgs.mpc_cli}/bin/mpc waitmessage info
              if [ $? != 0 ]; then
                sleep 1
              fi
            done
          ''}" "info"
        , Run Com "${config.scripts.power}" [] "power" 10
        , Run Com "${config.scripts.batt}" [] "bt" 50
        , Run Com "${config.scripts.playing}" [] "playing" 10
        , Run PipeReader "<test>:/home/infinisil/Test/xmobar/pipe" "testpipe"
      ]
      , sepChar = "%"
      , alignSep = "}{"
      , template = "%XMonadLog% } %info%  %playing% { ${optionalString config.mine.hardware.battery "%power%A  | "}%memory% | %dynnetwork%%cpu%  | ${optionalString config.mine.hardware.battery "%bt% | "}<fc=#ee9a00>%date%</fc>"
      }
  '';
in {

  options.mine.xmobar.enable = mkEnableOption "xmobar config";

  config = mkIf config.mine.xmobar.enable {

    scripts = {

      power = ''
        ${pkgs.bc}/bin/bc <<< "scale=1; $(cat /sys/class/power_supply/BAT0/current_now)/1000000"
      '';
      batt = ''
        PATH="${pkgs.acpi}/bin:${pkgs.gawk}/bin:${pkgs.bc}/bin:$PATH"

        battstat=$(acpi -b | cut -d' ' -f3 | tr -d ',')

        charge_now=$(cat /sys/class/power_supply/BAT0/charge_now)
        charge_full=$(cat /sys/class/power_supply/BAT0/charge_full)

        charge=$(bc <<EOF
        scale=2
        100 * $charge_now / $charge_full
        EOF
        )

        chargeInteger=$(printf "%.0f\n" "$charge")


        if [ $chargeInteger -le 0 ]; then
          chargeInteger=0
        elif [ $chargeInteger -ge 100 ]; then
          chargeInteger=100
        fi

        if [ $chargeInteger -le 12 ]; then
          symbol=
        elif [ $chargeInteger -le 37 ]; then
          symbol=
        elif [ $chargeInteger -le 62 ]; then
          symbol=
        elif [ $chargeInteger -le 87 ]; then
          symbol=
        else
          symbol=
        fi

        red=$(( 255 - $chargeInteger * 255 / 100 ))
        green=$(( $chargeInteger * 255 / 100 ))

        case $battstat in
        Full)
          ;;
        Discharging)
          postfix="-$(date -u -d $(acpi -b | cut -d' ' -f5) +"%Hh%M")"
          ;;
        Charging)
          postfix="+$(date -u -d $(acpi -b | cut -d' ' -f5) +"%Hh%M")"
          ;;
        *)
          ;;
        esac

        printf "<fc=#%02x%02x00>%s%% %s</fc> (%s)\n" "$red" "$green" "$charge" "$symbol" "$postfix"
      '';
      playing = ''
        status="$(systemctl --user is-active music)"
        if [ $status = active ]; then
          echo 
        else
          echo 
        fi
      '';

    };

    mine.xUserConfig = {

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

  };

}
