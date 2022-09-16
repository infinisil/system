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
    installPhase = ''
      mkdir -p $out/bin
      mv xmobar $out/bin/xmobar-custom
    '';
  };

  xmobar-power = pkgs.writeShellScriptBin "xmobar-power" ''
    ${pkgs.bc}/bin/bc <<< "scale=1; $(${pkgs.coreutils}/bin/cat /sys/class/power_supply/*/current_now)/1000000"
  '';
  xmobar-batt = pkgs.writeShellScriptBin "xmobar-batt" ''
    PATH="${lib.makeBinPath (with pkgs; [ acpi gawk bc coreutils ])}:$PATH"

    battstat=$(acpi -b | cut -d' ' -f3 | tr -d ',')

    charge_now=$(cat /sys/class/power_supply/*/charge_now)
    charge_full=$(cat /sys/class/power_supply/*/charge_full)

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
      xmobar-power
      pkgs.alsaUtils
      pkgs.mpc_cli
      pkgs.pulseaudio
    ];

  };
}

