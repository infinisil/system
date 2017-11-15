{ config, pkgs, ... }:
let
  keymap = pkgs.writeText "keymap.xkb" ''
    xkb_keymap {
      xkb_keycodes  { include "evdev+aliases(qwerty)"  };
      xkb_types     { include "complete"	};
      xkb_compat    { include "complete"	};
      xkb_symbols   {
        include "pc+us(dvp)+inet(evdev)+capslock(backspace)"
        key <LSGT> { [ Control_L ] };
        modifier_map Control { <LSGT> };
      };
      xkb_geometry  { include "pc(pc105)"	};
    };
  '';
in
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

    # When X started, load the customized one
    displayManager.sessionCommands = ''
      ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${keymap} $DISPLAY
      ${pkgs.xcape}/bin/xcape -e '#94=Escape' &
    '';
  };

  environment.systemPackages = [
    pkgs.xorg.xkbcomp
  ];

  environment.etc."X11/keymap.xkb".source = keymap;
}
