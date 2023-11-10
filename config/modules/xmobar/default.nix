{ lib, config, pkgs, ... }:
let
  inherit (lib) types;

  dependencies = p: [
    p.xmobar
  ];

  ghc = pkgs.haskellPackages.ghcWithPackages dependencies;

  xmobar-custom = pkgs.stdenv.mkDerivation {
    name = "xmobar-custom";
    unpackPhase = ''
      cp ${./xmobar.hs} xmobar.hs
    '';
    buildInputs = [ ghc ];
    buildPhase = ''
      ghc --make xmobar.hs -rtsopts -threaded
    '';
    script = ''
      #!${pkgs.runtimeShell}
      {
        echo "Now running.."
        while true; do
          ${placeholder "out"}/bin/xmobar-custom-unwrapped "$@"
          echo "Exited with code $?, restarting.."
        done
      } 2>&1 | \
        logger -t xmobar-custom
    '';
    passAsFile = [ "script" ];
    installPhase = ''
      mkdir -p $out/bin
      mv xmobar $out/bin/xmobar-custom-unwrapped
      cp "$scriptPath" $out/bin/xmobar-custom
      chmod +x $out/bin/xmobar-custom
    '';
  };

  xmobar-batt = pkgs.writeShellScriptBin "xmobar-batt" ''
    PATH="${lib.makeBinPath (with pkgs; [ jc jq acpi bc coreutils ])}:$PATH"

    shopt -s nullglob

    for supply in /sys/class/power_supply/BAT*; do
      cd "$supply"
      if [[ $(<type) != "Battery" ]]; then
        continue
      fi

      charge=$(bc <<EOF
      scale=2
      charge=100 * $(<charge_now) / $(<charge_full)
      if (charge > 100) {
        charge=100
      }
      if (charge < 0) {
        charge=0
      }
      charge
    EOF
      )

      level=$(bc <<EOF
      scale=0
      define round(f) {
        return (f + 0.5) / 1;
      }
      round($charge * 0.1)
    EOF
      )

      if [[ "$level" -le 2 ]]; then
        color="#fb4934"
      elif [[ "$level" -ge 8 ]]; then
        color="#b8bb26"
      else
        color="#fabd2f"
      fi

      case "$(<status)" in
        "Discharging")
          powered=
          ;;
        "Charging" | "Not charging" | "Full")
          powered=1
          ;;
      esac

      if [[ "$(bc <<< "$charge > 99.5")" == 1 ]]; then
        full=1
      fi

      unpowered_icons=(󰂎 󰁺 󰁻 󰁼 󰁽 󰁾 󰁿 󰂀 󰂁 󰂂 󰁹)
      powered_icons=(󰢟 󰢜 󰂆 󰂇 󰂈 󰢝 󰂉 󰢞 󰂊 󰂋 󰂅)

      icon=''${unpowered_icons["$level"]}

      #if [[ -n "$powered" ]]; then
      #  icon="$icon "
      #fi

      acpiResult=$(acpi | jc --acpi | jq '.[]')

      if [[ -n "$powered" ]]; then
        if [[ -n "$full" ]]; then
          suffix=""
        else
          hours=$(jq -r '.until_charged_hours' <<< "$acpiResult")
          minutes=$(jq -r '.until_charged_minutes' <<< "$acpiResult")
          time=$(printf "%dh%02d" "$hours" "$minutes")
          suffix=" <fc=#83a598>|</fc> 󰚥 ($time)"
        fi
      elif [[ -z "$full" ]] then
        hours=$(jq -r '.charge_remaining_hours' <<< "$acpiResult")
        minutes=$(jq -r '.charge_remaining_minutes' <<< "$acpiResult")
        time=$(printf "%dh%02d" "$hours" "$minutes")
        power_A=$(bc <<< "scale=2; $(<current_now) / 1000000")
        if [[ "$(bc <<< "$power_A <= 1.0")" == 1 ]]; then
          powerColor="#b8bb26"
        elif [[ "$(bc <<< "$power_A >= 1.6")" == 1 ]]; then
          powerColor="#fb4934"
        else
          powerColor="#fabd2f"
        fi
        suffix=" <fc=#83a598>|</fc> 󰚦 (<fc=$powerColor>$power_A</fc>A, $time)"
      else
        charge=100
        suffix=""
      fi

      printf "%s <fc=%s>%s</fc>%%%s <fc=#83a598>|</fc> \n" "$icon" "$color" "$charge" "$suffix"
    done
  '';
in {

  options.mine.xmobar = {
    enable = lib.mkEnableOption "xmobar";
    binary = lib.mkOption {
      readOnly = true;
      default = xmobar-custom;
    };
    shell = lib.mkOption {
      type = types.package;
      readOnly = true;
      default = pkgs.haskellPackages.shellFor {
        packages = p: [];
        # withHoogle = true;
        extraDependencies = p: {
          libraryHaskellDepends = dependencies p;
        };
        nativeBuildInputs = with pkgs.haskellPackages; [
          haskell-language-server
          hlint
        ];
      };
    };
  };

  config = lib.mkIf config.mine.xmobar.enable {

    environment.systemPackages = [
      xmobar-custom
      xmobar-batt
      pkgs.alsa-utils
      pkgs.mpc_cli
      pkgs.pulseaudio
    ];

  };
}

