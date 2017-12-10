{ config, pkgs, ... }:
{
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      nerdfonts
      hanazono
      ipafont
      mplus-outline-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts
      wqy_zenhei
      input-fonts
    ];
  };

  environment.systemPackages = with pkgs; [
    libnotify
    gnome3.gnome-font-viewer
    gnome3.gconf
    gnome3.gnome_terminal
    guake
    xclip
    evince
    vlc
    arandr
    xorg.xmessage
    lxappearance
    #arc-theme
    #gtk_engines
    gtk-engine-murrine
    compton
    #shotcut #video editor
    xbindkeys
    xwinwrap
    xbindkeys-config
    dmenu
    xlibs.xev
    deluge
    thunderbird
    haskellPackages.xmobar
  ];

  services = {
    gnome3.gnome-terminal-server.enable = true;

    physlock.enable = true;

    xserver = {
      enable = true;
      dpi = 96;
      exportConfiguration = true;
      wacom.enable = true;

      displayManager.sddm = {
        enable = true;
        autoLogin = {
          enable = true;
          user = "infinisil";
        };
      };

      displayManager.sessionCommands = ''
        # Set GTK_DATA_PREFIX so that GTK+ can find the themes
        export GTK_DATA_PREFIX=${config.system.path}

        # find theme engines
        export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
      '';
    };
  };
}
