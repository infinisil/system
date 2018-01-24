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
          "watch"
          "block_motd"
          "autoattach"
        ];
        channels = let
          detached = [
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
            "haskell-ide-engine"
            "openssh"
            "nixos-borg"
            "#linux"
            "gnupg"
            "zfsonlinux"
            "#dependent"
            "bash"
            "bottest"
            "tmux"
            "agda"
            "pulseaudio"
          ];
        in (map (c: {
          name = c;
          detached = true;
        }) detached) ++ [
          "nixos"
          "idris"
          "haskell"
        ];
        extraConf = ''
          RealName = Silvan Mosberger
          QuitMsg = Configuring ZNC, sorry for the join/quits!
        '';
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
