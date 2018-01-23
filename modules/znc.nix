{ config, ... }:

{

  users.users.infinisil.extraGroups = [ "znc" ];

  services.znc = {
    enable = true;
    openFirewall = true;
    mutable = false;
    confOptions = {
      nick = "infinisil";
      userName = "infinisil";
      extraUserConf = ''
        AutoClearChanBuffer = false
        AutoClearQueryBuffer = false
      '';
      networks.freenode = {
        userName = "infinisil";
        server = "chat.freenode.net";
        modules = [
          "sasl"
          "log"
          "savebuff ${config.private.passwords.znc-savebuff}"
        ];
        channels = [
          "nixos"
          "nixos-dev"
          "nixos-wiki"
          "nixos-borg"
          "emacs"
          "linux"
          "anime"
          "idris"
          "xmonad"
          "beets"
          "znc"
          "zsh"
          "purism"
          "youtube-dl"
          "bash"
          "mpd"
          "git"
          "gnupg"
          "crypto"
          "ffmpeg"
          "zfsonlinux"
          "weechat"
          "deluge"
          "ipfs"
          "openssh"
          "vim"
          "haskell"
          "ghc-mod"
          "openvpn"
          "agda"
          "#dependent"
          "alacritty"
          "bottest"
          "haskell-ide-engine"
          "pulseaudio"
          "tmux"
        ];
      };
      networks.mozilla = {
        userName = "infinisil";
        server = "irc.mozilla.org";
        modules = [
          "log"
          "savebuff ${config.private.passwords.znc-savebuff}"
        ];
        channels = [
          "firefox"
          "nightly"
          "rust"
        ];
      };
    };
  };

}
