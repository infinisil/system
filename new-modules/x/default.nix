{ config, lib, pkgs, ... }:

with lib;

{

  options.mine.x.enable = mkEnableOption "X config";

  config = mkIf config.mine.x.enable {

    mine.dunst.enable = true;

    services.physlock.enable = true;

    services.logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    services.xserver = {
      enable = true;
      dpi = 96;
      exportConfiguration = true;
      wacom.enable = true;

      displayManager = {
        sddm = {
          enable = true;
          autoLogin = {
            enable = false;
            user = "infinisil";
          };
        };

        sessionCommands = ''
          # Set GTK_DATA_PREFIX so that GTK+ can find the themes
          export GTK_DATA_PREFIX=${config.system.path}

          # find theme engines
          export GTK_PATH=${config.system.path}/lib/gtk-3.0:${config.system.path}/lib/gtk-2.0
        '';
      };
    };

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
      feh
      libnotify
      gnome3.gnome-font-viewer
      gnome3.gconf
      gnome3.gnome_terminal
      guake
      xclip
      evince
      vlc
      pcmanfm
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
      firefox
      thunderbird
      haskellPackages.xmobar
    ];


    mine.xUserConfig = {

      services.unclutter = {
        enable = true;
      };

      xsession.enable = true;

      home.packages = with pkgs; [
        mpv
        mine.pics
        mine.screenshot
        thunderbird
        helvetica-neue-lt-std
        firefox-nightly-bin
        mine.arcred
      ];

      programs.browserpass = {
        enable = true;
        browsers = [ "firefox" ];
      };


      services.random-background = {
        enable = true;
        imageDirectory = "%h/pics/wallpapers/desktop";
        interval = "20";
      };


      services.compton = {
        enable = true;
        fade = true;
        vSync = "opengl";
        backend = "xrender";
        activeOpacity = "0.95";
        inactiveOpacity = "0.80";
        extraOptions = ''
          no-fading-openclose = true;
          glx-swap-method = "buffer-age" # Fixes dpms standy resuming glitches
          paint-on-overlay = true; # Fixes xmobar not being visible when restarted, EDIT: It doesn't
          focus-exclude = [
            "override_redirect"
          ];
          glx-no-rebind-pixmap = true;
        '';
      };

      programs.zsh.shellAliases = {
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
        aniwp = "xwinwrap -ov -fs -ni -- mpv --loop=inf -wid WID --panscan=1";
      };

    };
  };

}
