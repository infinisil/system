{ config, pkgs, ... }:
{
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      nerdfonts
    ];
  };

  services = {
    compton = {
      enable = false;
      backend = "glx";
    };

    xserver = {
      enable = true;
      wacom.enable = true;

      #useXFS = true Is this needed?

      autoRepeatDelay = 250;
      autoRepeatInterval = 30;

      displayManager.slim.enable = true;
      displayManager.slim.defaultUser = "infinisil";
      displayManager.sessionCommands = ''
          # Set GTK_DATA_PREFIX so that GTK+ can find the themes
          export GTK_DATA_PREFIX=${config.system.path}

          # find theme engines
          export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
        '';

      desktopManager.default = "none";

      windowManager.default = "xmonad";
      windowManager.xmonad.enable = true;
      windowManager.xmonad.enableContribAndExtras = true;
      windowManager.xmonad.extraPackages = self: [ self.xmobar ];
      # from github.com/bernerdschaefer/dotfiles/blob/bs-nixos/nixos/configuration.nix
      multitouch = {
        enable = true;
        invertScroll = true;
        buttonsMap = [1 3 2];
      };
      synaptics = {
        #enable = true; # Only applies when multitouch is disabled
        #buttonsMap = [ 1 3 2 ];
        tapButtons = true;
        twoFingerScroll = true;
        horizTwoFingerScroll = true;
        scrollDelta = 10;
        minSpeed = "0.7";
        maxSpeed = "1.7";
        palmDetect = true;
        additionalOptions = ''
          Option "FingerHigh" "50"
          Option "FingerLow" "30"
          Option "TapAndDragGesture" "off"
          Option "TapButton1" "1"
          Option "TapButton2" "3"
          Option "TapButton3" "2"
          Option "VertScrollDelta" "-500"
          Option "HorizScrollDelta" "-500"
        '';
      };
    };
  };
}
