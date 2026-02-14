{ lib, config, pkgs, ... }: let inherit (lib) types; in {

  options.mine.xmonad = {
    enable = lib.mkEnableOption "xmonad";
    shell = lib.mkOption {
      type = types.package;
      readOnly = true;
      default =
        let cfg = config.home-manager.users.infinisil.xsession.windowManager.xmonad;
        in cfg.haskellPackages.shellFor {
          packages = p: [];
          # withHoogle = true;
          extraDependencies = p: {
            libraryHaskellDepends = cfg.extraPackages p ++ lib.optionals cfg.enableContribAndExtras [
              p.xmonad-contrib
              p.xmonad-extras
            ];
          };
          nativeBuildInputs = with cfg.haskellPackages; [
            haskell-language-server
            hlint
          ];
        };
    };
    locker = lib.mkEnableOption "locker";
    users = lib.mkOption {
      type = types.listOf types.str;
    };
  };

  config = lib.mkIf config.mine.xmonad.enable {

    mine.xmobar.enable = true;

    # Apparently needed for a home-manager-managed xsession
    services.xserver.desktopManager.xterm.enable = true;

    environment.systemPackages = [
      pkgs.lightlocker
    ];

    home-manager.users = lib.genAttrs config.mine.xmonad.users (user: {
      xsession.enable = true;
      xsession.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = self: [ self.fuzzy ];
        config = ./xmonad.hs;
      };
      xsession.initExtra = lib.mkIf config.mine.xmonad.locker ''
        light-locker --lock-on-suspend --lock-on-lid --lock-after-screensaver=30 &
      '';

      home.packages = let
        xmonadVolume = pkgs.writeScriptBin "xmonad-volume" ''
          export PATH=${lib.makeBinPath [ config.services.pulseaudio.package ]}
          case "$1" in
          lower)
            change="-2%"
            ;;
          raise)
            change="+2%"
            ;;
          *)
            echo "usage: xmonad-volume [raise|lower]" >&2
            exit 1
          esac
          pactl set-sink-mute @DEFAULT_SINK@ false
          pactl set-sink-volume @DEFAULT_SINK@ "$change"
        '';
        xmonadMute = pkgs.writeScriptBin "xmonad-mute" ''
          export PATH=${lib.makeBinPath [ config.services.pulseaudio.package ]}
          pactl set-sink-mute "@DEFAULT_SINK@" toggle
        '';
      in [
        xmonadVolume
        xmonadMute
        pkgs.rofi
      ];
    });

  };
}

