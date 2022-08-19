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
    modifier = lib.mkOption {
      type = types.enum [ 1 3 4 ];
    };
    switchWorkspaceKeys = lib.mkOption {
      type = types.listOf types.str;
    };
    shiftWorkspaceKeys = lib.mkOption {
      type = types.listOf types.str;
    };
  };

  config = lib.mkIf config.mine.xmonad.enable {

    mine.xmobar.enable = true;

    services.picom = {
      enable = true;
      experimentalBackends = true;
      backend = "glx";
      vSync = true;
      inactiveOpacity = 0.9;
      settings = {
        blur = {
          method = "gaussian";
          size = 10;
          deviation = 5.0;
        };
      };
    };

    # Apparently needed for a home-manager-managed xsession
    services.xserver.desktopManager.xterm.enable = true;

    mine.xUserConfig = {
      xsession.enable = true;
      xsession.windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = self: [ self.fuzzy ];
        config = let
          switchWorkspaceKeysVars = lib.listToAttrs (lib.imap0 (i: k: {
            name = "switch${toString i}";
            value = k;
          }) config.mine.xmonad.switchWorkspaceKeys);
          shiftWorkspaceKeysVars = lib.listToAttrs (lib.imap0 (i: k: {
            name = "shift${toString i}";
            value = k;
          }) config.mine.xmonad.shiftWorkspaceKeys);
        in pkgs.substituteAll ({
          name = "xmonad.hs";
          src = ./xmonad.hs;
          modifier = toString config.mine.xmonad.modifier;
        } // switchWorkspaceKeysVars // shiftWorkspaceKeysVars);
      };

      home.packages = let
        xmonadVolume = pkgs.writeScriptBin "xmonad-volume" ''
          export PATH=${lib.makeBinPath [ config.hardware.pulseaudio.package ]}
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
          export PATH=${lib.makeBinPath [ config.hardware.pulseaudio.package ]}
          pactl set-sink-mute "@DEFAULT_SINK@" toggle
        '';
      in [
        xmonadVolume
        xmonadMute
        pkgs.rofi
      ];
    };

  };
}

