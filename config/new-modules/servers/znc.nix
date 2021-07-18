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

    users.users = genAttrs config.mine.mainUsers (name: {
      extraGroups = [ "znc" ];
    });

    security.acme.certs.${config.networking.domain}.postRun = ''
      cp full.pem "${config.services.znc.dataDir}/znc.pem"
    '';

    services.znc = {
      enable = true;
      useLegacyConfig = false;
      openFirewall = true;
      mutable = false;
      modulePackages = with pkgs.zncModules; [
        playback
        backlog
        push
      ];
      config = {
        LoadModule = [ "playback" ];
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
            snoonet = {
              Server = "irc.snoonet.org 6667 ";
              LoadModule = cfg.defaultNetworkModules;
            };
          };
        };
      };
    };

  };

}
