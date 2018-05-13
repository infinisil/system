{ config, ... }: {

  mine.znc = {
    channels.freenode = [
      "nixos"
      "idris"
      "haskell"
      "haskell-ide-engine"
      "pijul"
      "nixos-borg"
      "nixos-chat"
      "#nixos-anime"
    ];
    detachedChannels.freenode = [
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
      "zfsonlinux"
      "#dependent"
      "bash"
      "bottest"
      "tmux"
      "#networking"
      "#programming"
    ];

    detachedChannels.tymoon = [
      "Stevenchan"
    ];

    detachedChannels.mozilla = [
      "rust"
      "rust-beginners"
    ];

    detachedChannels.twitch = [
      "baggers___"
      "emongg"
      "timthetatman"
      "ster"
      "xqcow"
      "aimbotcalvin"
    ];

    realName = "Silvan Mosberger";
    quitMsg = "Configuring ZNC, sorry for the join/quits!";
    nick = "infinisil";
    savebuffPassword = config.private.passwords.znc-savebuff;
  };

}
