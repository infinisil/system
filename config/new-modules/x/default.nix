{ config, lib, pkgs, ... }:

with lib;

{

  options.mine.x.enable = mkEnableOption "X config";

  config = mkIf config.mine.x.enable {

    programs.dconf.enable = true;

    mine.matrix.enable = true;

    mine.dunst.enable = true;

    mine.live-wallpaper.enable = true;


    services.logind.extraConfig = ''
      HandlePowerKey=poweroff
    '';

    services.xserver = {
      enable = true;
      dpi = 96;
      exportConfiguration = true;

      displayManager = {
        lightdm = {
          enable = true;
          background = toString config.mine.assets.blurred;
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
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        (nerdfonts.override {
          fonts = [
            "Iosevka"
            "FiraMono"
            "FantasqueSansMono"
          ];
        })
        hanazono
        ipafont
        mplus-outline-fonts.osdnRelease
        noto-fonts-cjk
        noto-fonts-emoji
        noto-fonts
        wqy_zenhei
      ];
    };

    environment.systemPackages = with pkgs; [
      feh
      libnotify
      xclip
      evince
      arandr
      lxappearance
      gtk_engines
      gtk-engine-murrine
      dmenu
      xorg.xev
      thunderbird
    ];


    mine.xUserConfig = {

      services.unclutter = {
        enable = true;
      };

      services.redshift.enable = true;
      services.redshift = {
        latitude = "47.4";
        longitude = "9.2";
      };

      xsession.enable = true;

      home.packages = with pkgs; [
        mpv
        thunderbird
        helvetica-neue-lt-std
      ];

      services.random-background = {
        enable = true;
        imageDirectory = "%h/pics/wallpapers/desktop";
        interval = "120";
      };

      programs.zsh.shellAliases = {
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
        aniwp = "xwinwrap -ov -fs -ni -- mpv --loop=inf -wid WID --panscan=1";
      };

    };
  };

}
