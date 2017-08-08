{ config, pkgs, ... }:
{
  imports = [
    ./keylayout.nix
  ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      nerdfonts
    ];
  };
  
  services = {
    
    redshift = {
      enable = true;
      latitude = "47";
      longitude = "9";
    };

    compton = {
      enable = false;
      backend = "glx";
    };

    gnome3.gnome-terminal-server.enable = true;

    xserver = {
      enable = true;
      wacom.enable = true;

      displayManager.slim.enable = true;
      displayManager.slim.defaultUser = "infinisil";
      displayManager.sessionCommands = ''
        # Set GTK_DATA_PREFIX so that GTK+ can find the themes
        export GTK_DATA_PREFIX=${config.system.path}

        # find theme engines
        export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
      '';
        
      desktopManager.default = "none";
      windowManager = {
        default = "xmonad";
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = hp: with hp; [
            xmobar
            fuzzy
          ];
        };
      };
    };
  };
}
