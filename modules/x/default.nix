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

    physlock = {
      enable = true;
      allowAnyUser = true;
    };

    logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

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

    home.file.".config/konsolerc".text = ''
      [Desktop Entry]
      DefaultProfile=Ancient.profile

      [Favorite Profiles]
      Favorites=

      [KonsoleWindow]
      ShowAppNameOnTitleBar=false
      ShowMenuBarByDefault=false

      [MainWindow]
      Height 1440=1403
      Height 720=691
      Height 800=763
      Height 900=863
      MenuBar=Disabled
      State=AAAA/wAAAAD9AAAAAAAACgAAAAWDAAAABAAAAAQAAAAIAAAACPwAAAAA
      ToolBarsMovable=Disabled
      Width 1280=632
      Width 1440=712
      Width 2560=845

      [Open-with settings]
      CompletionMode=5
      History=firefox %U,firefox

      [TabBar]
      TabBarVisibility=ShowTabBarWhenNeeded
    '';

    home.file.".local/share/konsole/Ancient.profile".text = ''
      [Appearance]
      AntiAliasFonts=true
      BoldIntense=true
      ColorScheme=Red
      Font=SauceCodePro Nerd Font,10,-1,5,50,0,0,0,0,0
      UseFontLineChararacters=false

      [Cursor Options]
      CursorShape=0

      [General]
      Environment=TERM=xterm-256color,COLORTERM=truecolor
      Name=Ancient
      Parent=FALLBACK/
      ShowTerminalSizeHint=false

      [Interaction Options]
      OpenLinksByDirectClickEnabled=true
      TripleClickMode=0
      UnderlineFilesEnabled=false

      [Keyboard]
      KeyBindings=default

      [Scrolling]
      HistoryMode=2
      ScrollBarPosition=2

      [Terminal Features]
      BlinkingCursorEnabled=true
    '';

    home.file.".local/share/konsole/Red.colorscheme".text = ''
      [Background]
      Color=43,43,41
      MaxRandomHue=0
      MaxRandomSaturation=0
      MaxRandomValue=0

      [BackgroundFaint]
      Color=0,0,0

      [BackgroundIntense]
      Color=87,87,67

      [Color0]
      Color=54,54,54

      [Color0Faint]
      Color=24,24,24

      [Color0Intense]
      Color=99,99,99

      [Color1]
      Color=195,117,97

      [Color1Faint]
      Color=101,25,0

      [Color1Intense]
      Color=219,102,73

      [Color2]
      Color=160,165,126

      [Color2Faint]
      Color=0,101,0

      [Color2Intense]
      Color=192,203,91

      [Color3]
      Color=209,163,117

      [Color3Faint]
      Color=101,74,0

      [Color3Intense]
      Color=221,155,88

      [Color4]
      Color=142,129,175

      [Color4Faint]
      Color=0,24,102

      [Color4Intense]
      Color=221,167,100

      [Color5]
      Color=171,113,126

      [Color5Faint]
      Color=95,5,95

      [Color5Intense]
      Color=210,100,96

      [Color6]
      Color=152,185,177

      [Color6Faint]
      Color=0,94,163

      [Color6Intense]
      Color=109,206,180

      [Color7]
      Color=208,208,208

      [Color7Faint]
      Color=101,101,101

      [Color7Intense]
      Color=249,249,249

      [Foreground]
      Color=219,195,165

      [ForegroundFaint]
      Color=205,0,0

      [ForegroundIntense]
      Color=227,171,102

      [General]
      Description=Ancient
      Opacity=1
      Wallpaper=
    '';

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
      pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
      pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
      aniwp = "xwinwrap -ov -fs -ni -- mpv --loop=inf -wid WID --panscan=1";
    };
  };
}

