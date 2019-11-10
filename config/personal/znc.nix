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
          "#haskell" = { };
          "#haskell-ide-engine" = { };
          "#haskell.nix" = { };
          "#nixos" = { };
          "##nixos-anime" = { };
          "#bottest" = { };
          "#nixos-chat" = { };
          "#idris" = { };
          "#nix-lang" = { };
          "#nixcon" = { Detached = true; };
          "#nixos-borg" = { };
          "#nixos-dev" = { };
          "#nixos-security" = { };
          "#minecraft" = { };
          "#home-manager" = { };
          "#pijul" = { Detached = true; };
          "#proglangdesign" = { };
          "#nixos-officehours" = { };
          "#nixos-officesupplies" = { };
          "#purism" = { };
          "#untrustix" = { };
          "#spectrum" = { };
          "#xmonad" = { };
        };
        Network.mozilla.Chan = {
          "#rust" = { Disabled = true; };
          "#rust-beginners" = { Disabled = true; };
        };
      };
    };
  };

}
