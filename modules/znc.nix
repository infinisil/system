{ pkgs, config, ... }:

let

  sharedNetworkModules = [
    "sasl"
    "log"
    "backlog"
    "watch"
    "block_motd"
    "autoattach"
    "savebuff ${config.private.passwords.znc-savebuff}"
  ];

  sharedExtraConf = ''
    RealName = Silvan Mosberger
    QuitMsg = Configuring ZNC, sorry for the join/quits!
  '';

in

{

  users.users.infinisil.extraGroups = [ "znc" ];

  services.znc = {
    enable = true;
    openFirewall = true;
    mutable = false;
    modulePackages = with pkgs.zncModules; [
      playback
      backlog
      push
    ];
    confOptions = {
      userModules = [ "push" ];
      modules = [ "playback" ];
      nick = "infinisil";
      userName = "infinisil";
      extraUserConf = ''
        AutoClearChanBuffer = false
        AutoClearQueryBuffer = false
      '';
      networks.freenode = {
        userName = "infinisil";
        server = "chat.freenode.net";
        modules = sharedNetworkModules;
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
            "cuberite"
            "shirakumo"
          ];
        in (map (c: {
          name = c;
          detached = true;
        }) detached) ++ [
          "nixos"
          "idris"
          "haskell"
          "haskell-ide-engine"
        ];
        extraConf = sharedExtraConf;
      };
      networks.mozilla = {
        userName = "infinisil";
        server = "irc.mozilla.org";
        modules = sharedNetworkModules;
        channels = map (c: {
          name = c;
          detached = true;
        }) [
          "firefox"
          "nightly"
          "rust"
          "rust-beginners"
        ];
        extraConf = sharedExtraConf;
      };
      networks.snoonet = {
        userName = "infinisil";
        server = "irc.snoonet.org";
        modules = sharedNetworkModules;
        extraConf = sharedExtraConf;
        port = 6667;
        useSSL = false;
      };
      networks.tymoon = {
        userName = "infinisil";
        server = "irc.tymoon.eu";
        modules = [
          "log"
          "backlog"
          "watch"
          "block_motd"
          "autoattach"
          "savebuff ${config.private.passwords.znc-savebuff}"
        ];
        extraConf = sharedExtraConf;
        useSSL = false;
        port = 6667;
        channels = [
          {
            name = "Stevenchan";
            detached = true;
          }
        ];
      };
    };
  };

}
