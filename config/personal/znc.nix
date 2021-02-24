{ lib, config, ... }:

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
        Network.rizon = lib.mkForce null;
        Network.twitch = lib.mkForce null;
        Network.freenode.Chan = {
          "#haskell.nix" = { };
          "#haskell" = { };
          "#nixos" = { };
          "##nixos-anime" = { };
          "#bottest" = { };
          "#nixos-chat" = { };
          "#nixos-gaming" = { };
          "#nixus" = { };
          "#nix-lang" = { };
          "#nixos-dev" = { };
          "#home-manager" = { };
        };
      };
    };
  };

}
