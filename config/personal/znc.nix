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
          "#pijul" = { };
          "#bottest" = { };
          "#nixos-chat" = { };
          "#idris" = { };
          "##classical" = { };
          "##crypto" = { Detached = true; };
          "##dependent" = { Detached = true; };
          "##linux" = { Detached = true; };
          "##networking" = { Detached = true; };
          "##programming" = { Detached = true; };
          "#alacritty" = { Detached = true; };
          "#anime" = { Detached = true; };
          "#bash" = { Detached = true; };
          "#beets" = { Detached = true; };
          "#deluge" = { Detached = true; };
          "#emacs" = { Detached = true; };
          "#ghc-mod" = { Detached = true; };
          "#git" = { Detached = true; };
          "#haskell-ide-engine" = { };
          "#ipfs" = { Detached = true; };
          "#mpd" = { Detached = true; };
          "#nixos-borg" = { Detached = true; };
          "#nixos-dev" = { Detached = true; };
          "#nixos-wiki" = { Detached = true; };
          "#openssh" = { Detached = true; };
          "#openvpn" = { Detached = true; };
          "#purism" = { Detached = true; };
          "#tmux" = { Detached = true; };
          "#vim" = { Detached = true; };
          "#weechat" = { Detached = true; };
          "#xmonad" = { Detached = true; };
          "#youtube-dl" = { Detached = true; };
          "#znc" = { Detached = true; };
          "#zsh" = { Detached = true; };
          "#zfsonlinux" = { Disabled = true; };
        };
        Network.mozilla.Chan = {
          "#rust" = { Detached = true; };
          "#rust-beginners" = { Detached = true; };
        };
        Network.twitch.Chan = {
          "#aimbotcalvin" = { Detached = true; };
          "#baggers___" = { Detached = true; };
          "#emongg" = { Detached = true; };
          "#ster" = { Detached = true; };
          "#timthetatman" = { Detached = true; };
          "#xqcow" = { Detached = true; };
        };
        Network.tymoon.Chan = {
          "#Stevenchan" = { Detached = true; };
        };
      };
    };
  };

}
