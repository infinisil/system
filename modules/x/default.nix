{ config, pkgs, lib, ... }:

with lib;

let

  home = config.home-manager.users.infinisil;

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

  pics = let
    action = pkgs.writeScript "action" ''
      if [ $(($2 > $3)) -eq 1 ]; then
        ${pkgs.feh}/bin/feh --bg-fill "$1"
        ln "$1" $HOME/pics/wallpapers/desktop-p
      else
        ln "$1" $HOME/pics/wallpapers/phone
        ${pkgs.rsync}/bin/rsync -avz --delete $HOME/pics/wallpapers/phone/ inf:/webroot/www/pwp
      fi
    '';
  in pkgs.writeScriptBin "pics" ''
    ${pkgs.feh}/bin/feh -.zZY -B black -A "${action} %F %w %h &" -D 3 ~/pics/anime
  '';
  screenshot = pkgs.writeScriptBin "screenshot" ''
    sleep 1
    ${pkgs.imagemagick}/bin/import -window root "$HOME/pics/screenshots/$(${pkgs.coreutils}/bin/date +%F-%T).png"
  '';

  thunderbird = pkgs.writeScriptBin "thunderbird" ''
    ${pkgs.thunderbird}/bin/thunderbird --profile $HOME/.config/thunderbird/cy89oqcv.default $@
  '';
in

{

  imports = [
    ./xmonad.nix
    ./xmobar.nix
    ./live-wallpaper.nix
    ./dunst.nix
  ];

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
    deluge
    firefox
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
          enable = false;
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

  home-manager.users.infinisil = {

    home.file.".profile".text = ''
      source ${home.programs.zsh.dotDir}/.zshenv
    '';

    xsession = {
      enable = true;
      profileExtra = ''
        ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${keymap} $DISPLAY
        ${pkgs.xcape}/bin/xcape -e '#94=Escape' &
      '';
    };

    home.keyboard.layout = "dvp";

    home.packages = with pkgs; [
      mpv
      pics
      screenshot
      konsole
      thunderbird
      helvetica-neue-lt-std
      firefox-nightly-bin
      (callPackage ./arcred.nix {})
      franz
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
      activeOpacity = "0.90";
      inactiveOpacity = "0.70";
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
      copy = "${pkgs.xclip}/bin/xclip -selection clipboard";
      paste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
      aniwp = "xwinwrap -ov -fs -ni -- mpv --loop=inf -wid WID --panscan=1";
    };
  };
}

