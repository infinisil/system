{ config, ... }:

let

  detach = name: { inherit name; detached = true; };

in

{

  mine.znc = {
    defaultNick = "infinisil";
    savebuffPassword = config.private.passwords.znc-savebuff;
  };

  services.znc = {
    confOptions = {
      nick = "infinisil";
      userName = "infinisil";
      passBlock = config.private.zncPassBlock;
      networks = {
        freenode.channels = [
          "nixos"
          "idris"
          "haskell"
          "haskell-ide-engine"
          "pijul"
          "nixos-borg"
          "nixos-chat"
          "#nixos-anime"
          "bottest"
        ] ++ map detach [
          "youtube-dl"
          "#crypto"
          "openvpn"
          "alacritty"
          "znc"
          "zsh"
          "emacs"
          "vim"
          "nixos-wiki"
          "xmonad"
          "nixos-dev"
          "deluge"
          "mpd"
          "anime"
          "weechat"
          "beets"
          "git"
          "purism"
          "ipfs"
          "ghc-mod"
          "openssh"
          "#linux"
          #"zfsonlinux"
          "#dependent"
          "bash"
          "tmux"
          "#networking"
          "#programming"
        ];

        tymoon.channels = [ (detach "Stevenchan") ];

        mozilla.channels = map detach [
          "rust"
          "rust-beginners"
        ];

        # Needs to be lower caps
        twitch.channels = map detach [
          "baggers___"
          "emongg"
          "timthetatman"
          "ster"
          "xqcow"
          "aimbotcalvin"
        ];

        gitter.password = config.private.passwords.gitterIrc;

        twitch.password = config.private.passwords.twitchChatOauth;

      };
    };
  };

}
