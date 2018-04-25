{ lib, config, pkgs, ... }:

with lib;

let
  keymap = pkgs.writeText "keymap.xkb" ''
    xkb_keymap {
      xkb_keycodes  { include "evdev+aliases(qwerty)"  };
      xkb_types     { include "complete"	};
      xkb_compat    { include "complete"	};
      xkb_symbols   {
        include "pc+us(dvp)+inet(evdev)+capslock(backspace)"
        key <LSGT> { [ Control_L ] };
        key <RWIN> { [ Break ] };
        modifier_map Control { <LSGT> };
      };
      xkb_geometry  { include "pc(pc105)"	};
    };
  '';
in

{

  options.mine.keylayout.enable = mkEnableOption "my keylayout config";

  config = mkIf config.mine.keylayout.enable (mkMerge [
    {

      # When in the DigitalOcean web console: `setxkbmap -layout us` (on local machine) and type in correct programmer dvorak
      i18n.consoleUseXkbConfig = true;

      services.xserver = {
        autoRepeatDelay = 200;
        autoRepeatInterval = 25;

        # Basic keymap, is used for i18n virtual consoles
        layout = "us";
        xkbVariant = "dvp";
        xkbOptions = "caps:backspace";

      };

      mine.xUserConfig = {
        home.keyboard.layout = "dvp";

        xsession.profileExtra = ''
          ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${keymap} $DISPLAY
          ${pkgs.xcape}/bin/xcape -e '#94=Escape' &
        '';
      };
    }
    (mkIf config.services.xserver.enable {
      services.xserver = {

        # When X started, load the customized one
        displayManager.sessionCommands = ''
          ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${keymap} $DISPLAY
        '';
      };

      environment.systemPackages = with pkgs; [
        pkgs.xorg.xkbcomp
      ];

      environment.etc."X11/keymap.xkb".source = keymap;

    })
  ]);

}
