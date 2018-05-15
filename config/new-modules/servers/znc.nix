{ pkgs, config, lib, ... }:

with lib;

let

  cfg = config.mine.znc;

in

{

  options.mine.znc = {

    enable = mkEnableOption "znc config";

    defaultNick = mkOption {
      type = types.str;
      description = "Default nick for networks";
    };

    defaultNetworkModules = mkOption {
      type = types.listOf types.str;
      description = "Default network modules, provided for your own use";
      default = [
        "sasl"
        "log"
        "backlog"
        "watch"
        "autoattach"
        "savebuff ${cfg.savebuffPassword}"
      ];
      readOnly = true;
    };

    defaultNetworkExtraConf = mkOption {
      type = types.lines;
      description = "Default network extra conf, provided for your own use";
      default = ''
        QuitMsg = Configuring ZNC, sorry for the joins/quits!
      '';
      readOnly = true;
    };

    savebuffPassword = mkOption {
      type = types.nullOr types.str;
      default = "null";
      description = "Password used for the savebuff module";
    };

  };

  config = mkIf cfg.enable {

    services.znc = {
      enable = true;
      openFirewall = true;
      mutable = false;
      modulePackages = with pkgs.zncModules; [
        playback
        backlog
        push
      ];
      confOptions = {
        userModules = [ "push" ];
        modules = [ "playback" ];
        extraUserConf = ''
          AutoClearChanBuffer = false
          AutoClearQueryBuffer = false
        '';
        networks = mapAttrs (net: attrs: ({
          userName = cfg.defaultNick;
          modules = cfg.defaultNetworkModules;
          extraConf = cfg.defaultNetworkExtraConf;
        } // attrs)) {

          freenode.server = "chat.freenode.net";
          gitter.server = "irc.gitter.im";
          mozilla.server = "irc.mozilla.org";

          snoonet = {
            server = "irc.snoonet.org";
            port = 6667;
            useSSL = false;
          };

          tymoon = {
            server = "irc.tymoon.eu";
            useSSL = false;
            port = 6667;
          };

          twitch = {
            server = "irc.chat.twitch.tv";
            modules = [ "autoattach" ];
          };

          rizon = {
            server = "irc.rizon.net";
            port = 6667;
            useSSL = false;
          };

        };
      };
    };

  };

}
