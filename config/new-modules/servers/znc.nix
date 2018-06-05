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
        (mkIf (cfg.savebuffPassword != null) "savebuff ${cfg.savebuffPassword}")
      ];
      readOnly = true;
    };

    savebuffPassword = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Password used for the savebuff module";
    };

    twitchPassword = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Password used for twitch";
    };

    gitterPassword = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Password used for gitter";
    };
  };

  config = mkIf cfg.enable {

    services.znc = {
      enable = true;
      openFirewall = true;
      mutable = false;
      config = {
        LoadModule = [ "playback" ];
        Listener.l = {
          Port = 5000;
          IPv4 = true;
          IPv6 = true;
          SSL = true;
        };
        User.${cfg.defaultNick} = {
          Admin = mkDefault true;
          LoadModule = [ "push" ];
          QuitMsg = mkDefault "Configuring ZNC, sorry for the joins/quits!";
          Nick = mkDefault cfg.defaultNick;
          AltNick = mkDefault "${cfg.defaultNick}_";
          Ident = mkDefault cfg.defaultNick;
          AutoClearChanBuffer = false;
          AutoClearQueryBuffer = false;

          Network = {
            freenode = {
              Server = "chat.freenode.net +6697";
              LoadModule = cfg.defaultNetworkModules;
            };
            gitter = mkIf (cfg.gitterPassword != null) {
              Server = "irc.gitter.im +6697 ${cfg.gitterPassword}";
              LoadModule = cfg.defaultNetworkModules;
            };
            mozilla = {
              Server = "irc.mozilla.org +6697";
              LoadModule = cfg.defaultNetworkModules;
            };
            snoonet = {
              Server = "irc.snoonet.org 6667 ";
              LoadModule = cfg.defaultNetworkModules;
            };
            tymoon = {
              Server = "irc.tymoon.eu 6667";
              LoadModule = cfg.defaultNetworkModules;
            };
            twitch = mkIf (cfg.twitchPassword != null) {
              Server = "irc.chat.twitch.tv +6697 ${cfg.twitchPassword}";
              LoadModule = [ "autoattach" ];
            };
            rizon = {
              Server = "irc.rizon.net 6667";
              LoadModule = cfg.defaultNetworkModules;
            };

          };
        };
      };
    };

  };

}
