{ config, pkgs, lib, ... }:

with lib;

{

  options = {
    mine.wm.enable = mkEnableOption "My window manager";
  };

  config = mkIf config.mine.wm.enable {

    mine.terminal.enable = true;

    nixpkgs.overlays = [
      (import ../../../nixpkgs-mozilla/firefox-overlay.nix)
    ];

    scripts = let
      pactl = "${pkgs.pulseaudioLight}/bin/pactl";
      mpc = "MPD_HOST=${config.environment.variables.MPD_HOST} ${pkgs.mpc_cli}/bin/mpc";

      vol = up: ''
        ${pactl} set-sink-mute @DEFAULT_SINK@ false
        ${pactl} set-sink-volume @DEFAULT_SINK@ ${if up then "+" else "-"}2%
      '';
      bright = up: ''
        ${pkgs.xorg.xbacklight}/bin/xbacklight ${if up then "+" else "-"} 10
      '';
      toggleService = user: service: let
        systemctl = "systemctl" + optionalString user " --user";
      in ''
        if [ $(${systemctl} is-active ${service}) = active ]; then
          ${systemctl} stop ${service}
        else
          ${systemctl} start ${service}
        fi
      '';
    in {
      volUp = vol true;
      volDown = vol false;
      brightUp = bright true;
      brightDown = bright false;
      nope = "${mpc} sendmessage nope nope";
      toggleCompton = toggleService true "compton";
      tag = ''
        mkdir -p $HOME/.local/share/mtags
        cd $HOME/.local/share/mtags
        ${pkgs.fd}/bin/fd -t f | \
          xargs stat --printf "%Z\t%n\n" | \
          sort -r | cut -f2 | \
          ${pkgs.dmenu}/bin/dmenu -l 10 | \
            xargs ${pkgs.writeScript "mkdirTag" ''
              #!${pkgs.bash}/bin/bash
              mkdir -p "$(dirname "$1")"
              touch "$1"
              ${mpc} sendmessage tag "$1"
            ''}
      '';
      toggleXmobar = toggleService true "xmobar";
      toggleLive = toggleService true "live-wallpaper";
      next = "${mpc} next";
      prev = "${mpc} prev";
      emacs = ''
        if ! [ $(systemctl --user is-active emacs) = active ]; then
          systemctl --user start emacs
        fi
        emacsclient -c -n
      '';
      toggleMute = "${pactl} set-sink-mute @DEFAULT_SINK@ toggle";
      playpause = toggleService true "music";
      firefox = "${pkgs.firefox-nightly-bin}/bin/firefox";
      #terminal = "${pkgs.alacritty}/bin/alacritty";
      terminal = "${pkgs.konsole}/bin/konsole";
      zpool = "${pkgs.zfs}/bin/zpool";
      dmenu_run = "${pkgs.dmenu}/bin/dmenu_run";

    } // listToAttrs (map (n: {
      name = "rate${toString n}";
      value = "${mpc} sendmessage rating ${toString n}";
    }) (range 1 10));

    mine.xUserConfig = {

      xsession.windowManager.xmonad = {
        enable = true;
        extraPackages = self: [ self.fuzzy ];
        enableContribAndExtras = true;
        config = pkgs.runCommand "xmonad.hs" config.scripts
          "substituteAll ${./xmonad.hs} $out";

      };

    };
  };
}
