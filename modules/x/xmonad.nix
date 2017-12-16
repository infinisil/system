{ config, pkgs, lib, ... }:

with import ../../scripts { inherit config pkgs lib; };


let
  home = config.home-manager.users.infinisil;

  vol = up: pkgs.writeScript "vol${if up then "up" else "down"}" ''
    ${pkgs.pulseaudioLight}/bin/pactl set-sink-mute @DEFAULT_SINK@ false
    ${pkgs.pulseaudioLight}/bin/pactl set-sink-volume @DEFAULT_SINK@ ${if up then "+" else "-"}2%
  '';

  brightness = up: pkgs.writeScript "brightness${if up then "up" else "down"}" ''
    ${pkgs.xorg.xbacklight}/bin/xbacklight ${if up then "+" else "-"} 10
  '';

  nope = pkgs.writeScript "nope" ''
    #!${pkgs.bash}/bin/bash
    export MPD_HOST=${home.home.sessionVariables.MPD_HOST}

    ${pkgs.mpc_cli}/bin/mpc sendmessage nope nope
  '';

  toggleCompton = pkgs.writeScript "toggle-compton" ''
    #!${pkgs.bash}/bin/bash
    if [ $(systemctl --user is-active compton) = active ]; then
      systemctl --user stop compton
    else
      systemctl --user start compton
    fi
  '';

  setRating = n: pkgs.writeScript "set-rating-${toString n}" ''
    #!${pkgs.bash}/bin/bash
    export MPD_HOST=${home.home.sessionVariables.MPD_HOST}

    ${pkgs.mpc_cli}/bin/mpc sendmessage rating "${toString n}"
  '';

  tag = pkgs.writeScript "tag" ''
    #!${pkgs.bash}/bin/bash
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
          ${pkgs.mpc_cli}/bin/mpc sendmessage tag "$1"
        ''}
  '';

  toggleXmobar = pkgs.writeScript "toggle-xmobar" ''
    #!${pkgs.bash}/bin/bash
    if [ $(systemctl --user is-active xmobar) = active ]; then
      systemctl --user stop xmobar
    else
      systemctl --user start xmobar
    fi
  '';

  emacs = pkgs.writeScript "emacs" ''
    #!${pkgs.bash}/bin/bash
    if ! [ $(systemctl --user is-active emacs) = active ]; then
      systemctl --user start emacs
    fi
    emacsclient -c -n
  '';


  env = {
    konsole = "${pkgs.konsole}/bin/konsole";
    firefox = "${pkgs.firefox-nightly-bin}/bin/firefox";
    zpool = "${pkgs.zfs}/bin/zpool";
    dmenu_run = "${pkgs.dmenu}/bin/dmenu_run";
    inherit (pkgs) mpc_cli;
    inherit playpause toggleCompton tag toggleXmobar nope emacs;
    inherit (config.private.scripts) toggle desktop mobile nani explosion;
    volup = vol true;
    voldown = vol false;
    brightup = brightness true;
    brightdown = brightness false;
    mutetoggle =
    "${pkgs.pulseaudioLight}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
  } // builtins.listToAttrs (map (n: {
    name = "rate${toString n}";
    value = setRating n;
  }) (lib.range 1 10));

in {

  nixpkgs.overlays = [
    (import ../../nixpkgs-mozilla/firefox-overlay.nix)
  ];

  home-manager.users.infinisil = {

    home.sessionVariables = {
      XMONAD_CONFIG_DIR = "${home.home.sessionVariables.XDG_CONFIG_HOME}/xmonad";
      XMONAD_CACHE_DIR = "${home.home.sessionVariables.XDG_CACHE_HOME}/xmonad";
      XMONAD_DATA_DIR = "${home.home.sessionVariables.XDG_DATA_HOME}/xmonad";
    };

    xsession.windowManager.xmonad = {
      enable = true;
      extraPackages = self: [ self.fuzzy ];
      enableContribAndExtras = true;
      config = pkgs.runCommand "xmonad.hs" env ''
        substituteAll ${./xmonad.hs} $out
      '';
    };
  };
}
