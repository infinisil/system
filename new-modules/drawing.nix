{ lib, config, pkgs, ... }:

with lib;

let

  wacomScript = pkgs.writeScriptBin "wacom-setup" ''
    #!${pkgs.stdenv.shell}

  '';

in

{

  options.mine.drawing.enable = mkEnableOption "drawing config";

  config = mkIf config.mine.drawing.enable {

    services.xserver.wacom.enable = true;

    environment.systemPackages = with pkgs; [
      krita
    ];

    mine.xUserConfig = {
      systemd.user.services.wacom = {
        Unit = {
          Description = "Wacom setup";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          ExecStart = pkgs.writeScript "wacom" ''
            #!${pkgs.stdenv.shell}
            PATH="${lib.makeBinPath [ pkgs.xf86_input_wacom ]}:$PATH"
            n=0
            while [ -z "$(xsetwacom list)" ]; do
              n=$(( $n+1 ))
              if [ $n -eq 10 ]; then
                echo "No wacom device found within 5 seconds"
                exit 0
              fi
              sleep 0.5
            done

            echo "Wacom device found, setting properties.."

            set -x
            xsetwacom set "Wacom Intuos PT S Pad pad" button 1 "key ctrl z"
          '';
          Type = "oneshot";
        };
        Install.WantedBy = [ "graphical-session.target" ];
      };
    };

    # Workaround for https://github.com/systemd/systemd/issues/7587
    # When the bug is fixed, the ACTION=="add" can be removed and the full "remove" rule too
    services.udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="usb", ATTRS{idVendor}=="056a", ATTRS{idProduct}=="0302", TAG+="systemd", ENV{SYSTEMD_USER_WANTS}="wacom.service"
      ACTION=="remove", SUBSYSTEM=="usb", ENV{PRODUCT}=="56a/302/*", TAG+="systemd"
    '';
  };

}
