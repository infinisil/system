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
          "#haskell-ide-engine" = { };
          "#haskell.nix" = { };
          "#nixos" = { };
          "##nixos-anime" = { };
          "#bottest" = { };
          "#nixos-chat" = { };
          "#nix-lang" = { };
          "#nixos-borg" = { };
          "#nixos-dev" = { };
          "#nixos-security" = { };
          "#minecraft" = { };
          "#home-manager" = { };
          "#nixos-officehours" = { };
        };
      };
    };
  };

}
