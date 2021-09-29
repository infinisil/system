{ config, pkgs, lib, ... }:

with lib;

let

  musicInfo = pkgs.writers.writePython3 "musicInfo" {
    libraries = [ pkgs.python3.pkgs.mpd2 ];
  } ../musicInfo.py;

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
          [ "-t" , "Up: <tx> KB/s, Down: <rx> KB/s | "
          , "-L" , "10000"
          , "-H" , "500000"
          , "-l" , "green"
          , "-n" , "orange"
          , "-h" , "red" ] 10
        , Run CommandReader "${pkgs.writeScript "enoping" ''
            #!${pkgs.bash}/bin/bash
            while true; do
              if /run/wrappers/bin/ping -I eno1 -c 1 -w 1 1.1.1.1 >/dev/null; then
                echo "up"
              else
                echo "down"
              fi
              sleep 10
            done
          ''}" "enoping"
        , Run Com "${config.scripts.power}" [] "power" 10
        , Run Com "${config.scripts.batt}" [] "bt" 50
        , Run Com "${pkgs.writeShellScript "xmobar-volume" ''
          export PATH=${lib.makeBinPath [ config.hardware.pulseaudio.package pkgs.gnused pkgs.gawk ]}
          currentName=$(pacmd dump | sed -n 's/set-default-sink \(.*\)/\1/p')

          while IFS=$'\t' read -r number name descr volume mute; do
            if [[ "$name" == "$currentName" ]]; then
              odescr=''${descr%% *}
              ovolume=$volume
              omuted=$mute
              break
            fi
          done < <(pactl list sinks | awk -F '\t' '
            match($0, "Sink #(.*)", a) {
              number=a[1]
            }
            match($0, "\tDescription: (.*)", a) {
              descr=a[1]
            }
            match($0, "\tDriver: (.*)", a) {
              driver=a[1]
            }
            match($0, "\tName: (.*)", a) {
              name=a[1]
            }
            match($0, "\tMute: (.*)", a) {
              mute=a[1]
            }
            match($0, "\tVolume:.* ([0-9]+)%.* ([0-9]+)%", a) {
              if (driver != "module-null-sink.c") {
                print number "\t" name "\t" descr "\t" ((a[1] + a[2]) / 2) "\t" mute
              }
            }
          ')

          currentInput=$(pacmd dump | sed -n 's/set-default-source \(.*\)/\1/p')

          while IFS=$'\t' read -r name mute; do
            if [[ "$name" == "$currentInput" ]]; then
              imuted=$mute
              break
            fi
          done < <(pactl list sources | awk -F '\t' '
            match($0, "Source #(.*)", a) {
              number=a[1]
            }
            match($0, "\tDriver: (.*)", a) {
              driver=a[1]
            }
            match($0, "\tName: (.*)", a) {
              name=a[1]
            }
            match($0, "\tMute: (.*)", a) {
              if (driver != "module-null-sink.c") {
                print name "\t" a[1]
              }
            }
          ')

          if [[ "$omuted" == "$imuted" ]]; then
            if [[ "$omuted" == yes ]]; then
              muteString=" [muted]"
            else
              muteString=""
            fi
          else
            if [[ "$omuted" == yes ]]; then
              muteString=" [only output muted]"
            else
              muteString=" [only input muted]"
            fi
          fi

          echo "<action=pot>$odescr</action> | Vol: $ovolume%$muteString"

        ''}" [] "volume" 2
      , Run CommandReader "${musicInfo}" "info"
      ]
      , sepChar = "%"
      , alignSep = "}{"
      , template = "%XMonadLog% }%info%{ ${optionalString config.mine.hardware.battery "%power%A  | "}%volume% | %memory% | eno1 %enoping% | %dynnetwork%%cpu%  | ${optionalString config.mine.hardware.battery "%bt% | "}<fc=#ee9a00>%date%</fc>"
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
        PATH="${lib.makeBinPath (with pkgs; [ acpi gawk bc coreutils ])}:$PATH"

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

    };

    mine.xUserConfig = {

      systemd.user.services.xmobar = {
        Unit = {
          Description = "Xmobar";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          Environment = "PATH=${lib.makeBinPath [ pkgs.mpc_cli pkgs.pot pkgs.pulseaudio pkgs.gnused ]}";
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
