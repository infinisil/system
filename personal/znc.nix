{ config, ... }: {

  mine.znc = {
    channels.freenode = [
      "nixos"
      "idris"
      "haskell"
      "haskell-ide-engine"
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
      "ffmpeg"
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
      "nixos-borg"
      "nixos-chat"
      "#linux"
      "gnupg"
      "zfsonlinux"
      "#dependent"
      "bash"
      "bottest"
      "tmux"
      "agda"
      "pulseaudio"
      "cuberite"
      "shirakumo"
    ];

    channels.tymoon = [
      "Stevenchan"
    ];

    detachedChannels.mozilla = [
      "firefox"
      "nightly"
      "rust"
      "rust-beginners"
    ];

    realName = "Silvan Mosberger";
    quitMsg = "Configuring ZNC, sorry for the join/quits!";
    nick = "infinisil";
    savebuffPassword = config.private.passwords.znc-savebuff;
  };

}
