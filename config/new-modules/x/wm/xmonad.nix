{ config, pkgs, lib, ... }:

with lib;

let

  weechat = pkgs.weechat.override {
    configure = { availablePlugins, ... }: with pkgs.weechatScripts; {
      plugins = lib.attrValues (availablePlugins // {
        python = availablePlugins.python.withPackages (_: [ weechat-matrix ]);
      });
      scripts = [ weechat-matrix ];
    };
  };

in

{

  options = {
    mine.wm.enable = mkEnableOption "My window manager";
  };

  config = mkIf config.mine.wm.enable {

    mine.xmobar.enable = true;
    mine.taffybar.enable = false;

    mine.terminal.enable = true;

    scripts = let
      pactl = "${pkgs.pulseaudioLight}/bin/pactl";
      mpc = "MPD_HOST=${config.mine.mpdHost} ${pkgs.mpc_cli}/bin/mpc";

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
      toggleMute = ''
        export PATH=${lib.makeBinPath [ config.hardware.pulseaudio.package pkgs.gnused pkgs.gawk ]}
        currentName=$(pacmd dump | sed -n 's/set-default-sink \(.*\)/\1/p')

        while IFS=$'\t' read -r number name descr volume mute; do
          if [[ "$name" == "$currentName" ]]; then
            if [[ "$mute" == "yes" ]]; then
              pactl set-source-mute @DEFAULT_SOURCE@ 0
              pactl set-sink-mute @DEFAULT_SINK@ 0
            else
              pactl set-source-mute @DEFAULT_SOURCE@ 1
              pactl set-sink-mute @DEFAULT_SINK@ 1
            fi
            exit 0
          fi
        done < <(pactl list sinks | awk -F '\t' '
          match($0, "Sink #(.*)", a) {
            number=a[1]
          }
          match($0, "\tDescription: (.*)", a) {
            descr=a[1]
          }
          match($0, "\tDriver: (.*)", a) {
            driver=a[1]
          }
          match($0, "\tName: (.*)", a) {
            name=a[1]
          }
          match($0, "\tMute: (.*)", a) {
            mute=a[1]
          }
          match($0, "\tVolume:.* ([0-9]+)%.* ([0-9]+)%", a) {
            if (driver != "module-null-sink.c") {
              print number "\t" name "\t" descr "\t" ((a[1] + a[2]) / 2) "\t" mute
            }
          }
        ')
      '';
      playpause = toggleService true "music";
      firefox = "${config.mine.firefox}/bin/firefox";
      terminal = config.mine.terminal.binary;
      irc = "exec ${config.mine.terminal.binary} -e ${weechat}/bin/weechat";
      zpool = "${pkgs.zfs}/bin/zpool";
      run = "${pkgs.rofi}/bin/rofi -show run -theme gruvbox-dark";

    } // listToAttrs (map (n: {
      name = "rate${toString n}";
      value = "${mpc} sendmessage rating ${toString n}";
    }) (range 1 10));

    mine.userConfig = {

      home.packages = [ weechat ];

    };

    mine.xUserConfig = {

      xsession.windowManager.xmonad = {
        enable = true;
        extraPackages = self: [ self.fuzzy ];
        enableContribAndExtras = true;
        config = pkgs.runCommand "xmonad.hs" (config.scripts // {
          spacing = if config.mine.hardware.battery then "False" else "True";
        }) "substituteAll ${./xmonad.hs} $out";

      };

    };
  };
}
