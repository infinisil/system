{ config, ... }:

{

  mine.znc = {
    defaultNick = "infinisil";
    savebuffPassword = config.private.passwords.znc-savebuff;
    twitchPassword = config.private.passwords.twitchChatOauth;
    gitterPassword = config.private.passwords.gitterIrc;
  };

  services.znc = {
    config = {
      User.infinisil = {
        AltNick = "infinisi1";
        RealName = "Silvan Mosberger";
        Network.freenode.Chan = {
          "#haskell" = { };
          "#nixos" = { };
          "##nixos-anime" = { };
          "#bottest" = { };
          "#nixos-chat" = { };
          "#idris" = { };
          "##classical" = { };
          "#nix-lang" = { };
          "##apoptosis" = { Detached = true; };
          "#nixcon" = { Detached = true; };
          "##linux" = { Detached = true; };
          "#anime" = { Detached = true; };
          "#nixos-borg" = { Detached = true; };
          "#nixos-dev" = { Detached = true; };
          "#nixos-wiki" = { Detached = true; };
          "#nixos-nur" = { Detached = true; };
          "#zfsonlinux" = { Disabled = true; };
        };
        Network.mozilla.Chan = {
        };
        Network.twitch.Chan = {
          "#aimbotcalvin" = { Detached = true; };
          "#baggers___" = { Detached = true; };
          "#emongg" = { Detached = true; };
          "#ster" = { Detached = true; };
          "#timthetatman" = { Detached = true; };
          "#xqcow" = { Detached = true; };
        };
      };
    };
  };

}
