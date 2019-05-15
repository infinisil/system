{ config, lib, pkgs, ... }: {

  imports = [
    ./keys.nix
    ./znc.nix
    ./wlan.nix
    ./bins.nix
  ];

  mine.keylayout = {
    layoutFile = ./keymap.xkb;
    xcapeConfig."#94" = "Escape";
  };

  networking.extraHosts = ''
    192.168.178.1 fritz.box
  '';


  mine.dns.ipnsHash = "QmcF3xqxFZxDLBJ5fNmr8vZ5p83SoS5zuavYMhizh2L1dp";

  mine.openvpn.server = {
    subnet = "10.149.76.0";
    fixedClients = builtins.removeAttrs config.networking.connectivitySpec.vpn [ "protos" ] // {
      iphone = "10.149.76.4";
    };
  };

  mine.xUserConfig = {
    services.redshift = {
      latitude = "47.4";
      longitude = "9.2";
    };
  };

  mine.newsboat = {
    config = ''
      auto-reload yes
      browser "firefox %u"
      bind-key j down
      bind-key k up
      bind-key h quit
      bind-key l open
      bind-key o open-in-browser-and-mark-read
      bind-key O open-all-unread-in-browser-and-mark-read
      reload-time 10
      notify-program "${lib.getBin pkgs.libnotify}/bin/notify-send"
    '';
    urls = [
      http://kamalmarhubi.com/blog/feed.xml
      https://www.youtube.com/feeds/videos.xml?user=EthosLab
      https://www.youtube.com/feeds/videos.xml?user=Pyropuncher
      http://degoes.net/feed.xml
      https://cli.fan/posts/index.xml
    ];
  };

}
