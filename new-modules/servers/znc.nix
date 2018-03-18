{ pkgs, config, lib, ... }:

with lib;

let

  cfg = config.mine.znc;

  sharedNetworkModules = [
    "sasl"
    "log"
    "backlog"
    "watch"
    "block_motd"
    "autoattach"
    "savebuff ${cfg.savebuffPassword}"
  ];

  sharedExtraConf = concatStringsSep "\n" (
    (optional (cfg.realName != null) "RealName = ${cfg.realName}") ++
    (optional (cfg.quitMsg != null) "QuitMsg = ${cfg.quitMsg}")
  );

in

{

  options.mine.znc = {

    enable = mkEnableOption "znc config";

    detachedChannels = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      description = "Attribute set of which channels should be joined detached for each network";
    };

    channels = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      description = "Attribute set of which channels should be joined non-detached for each network";
    };

    realName = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Real name for networks";
    };

    quitMsg = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Quit message for networks";
    };

    nick = mkOption {
      type = types.str;
      description = "Nick and username";
    };

    savebuffPassword = mkOption {
      type = types.nullOr types.str;
      default = "null";
      description = "Password used for the savebuff module";
    };

  };

  config = mkIf cfg.enable {

    users.users = mkMerge (map (user: {
      ${user}.extraGroups = [ "znc" ];
    }) config.mine.mainUsers);

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
        nick = cfg.nick;
        userName = cfg.nick;
        extraUserConf = ''
          AutoClearChanBuffer = false
          AutoClearQueryBuffer = false
        '';
        networks.freenode = {
          userName = cfg.nick;
          server = "chat.freenode.net";
          modules = sharedNetworkModules;
          channels = cfg.channels.freenode or [] ++
            map (c: {
              name = c;
              detached = true;
            }) (cfg.detachedChannels.freenode or []);
          extraConf = sharedExtraConf;
        };
        networks.mozilla = {
          userName = cfg.nick;
          server = "irc.mozilla.org";
          modules = sharedNetworkModules;
          channels = cfg.channels.mozilla or [] ++
            map (c: {
              name = c;
              detached = true;
            }) (cfg.detachedChannels.mozilla or []);
          extraConf = sharedExtraConf;
        };
        networks.snoonet = {
          userName = cfg.nick;
          server = "irc.snoonet.org";
          modules = sharedNetworkModules;
          channels = cfg.channels.snoonet or [] ++
            map (c: {
              name = c;
              detached = true;
            }) (cfg.detachedChannels.snoonet or []);
          extraConf = sharedExtraConf;
          port = 6667;
          useSSL = false;
        };
        networks.tymoon = {
          userName = cfg.nick;
          server = "irc.tymoon.eu";
          modules = sharedNetworkModules;
          channels = cfg.channels.tymoon or [] ++
            map (c: {
              name = c;
              detached = true;
            }) (cfg.detachedChannels.tymoon or []);
          extraConf = sharedExtraConf;
          useSSL = false;
          port = 6667;
        };
        networks.twitch = {
          userName = "infinisil";
          password = config.private.passwords.twitchChatOauth;
          server = "irc.chat.twitch.tv";
          modules = [
            "autoattach"
          ];
          channels = cfg.channels.twitch or [] ++
            map (c: {
              # Twitch channels don't work correctly with uppercase
              name = toLower c;
              detached = true;
            }) (cfg.detachedChannels.twitch or []);
          extraConf = sharedExtraConf;
        };
      };
    };

  };

}
