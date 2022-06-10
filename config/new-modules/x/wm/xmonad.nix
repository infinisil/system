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
      pactl = "${pkgs.pulseaudio}/bin/pactl";
      vol = up: ''
        ${pactl} set-source-mute @DEFAULT_SOURCE@ false
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
      toggleCompton = toggleService true "compton";
      toggleXmobar = toggleService true "xmobar";
      toggleLive = toggleService true "live-wallpaper";
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
      firefox = "${config.mine.firefox}/bin/firefox";
      irc = "exec kitty -e weechat";
      zpool = "${pkgs.zfs}/bin/zpool";
      run = "${pkgs.rofi}/bin/rofi -show run -theme gruvbox-dark";
    };

    mine.userConfig = {

      home.packages = [ weechat ];

    };

    mine.xUserConfig = {

      xsession.windowManager.xmonad = {
        enable = true;
        extraPackages = self: [ self.fuzzy ];
        enableContribAndExtras = true;
        config = pkgs.runCommand "xmonad.hs" config.scripts "substituteAll ${./xmonad.hs} $out";

      };

    };
  };
}
