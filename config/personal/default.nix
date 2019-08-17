{ config, lib, pkgs, ... }: {

  imports = [
    ./keys.nix
    ./znc.nix
    ./wlan.nix
    ./bins.nix
    ./user.nix
  ];

  time.timeZone = "Europe/Zurich";

  mine.keylayout = {
    enable = true;
    layoutFile = ./keymap.xkb;
    xcapeConfig."#94" = "Escape";
  };

  networking.extraHosts = ''
    192.168.178.1 fritz.box
  '';

  mine.dns.dkimKey = "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDqEBkfzMeMpXcHmMnasi5sE98SGIphwuWMHFmtXAtqGKsr8gjOQ5rZLTRhqOZR2CZc6xY2iCBtQ6nxFOHfJ/UW5tNanvi2nuo4jhrq9+ZNupdsKwxDpBNm7W9HVO2a0FP6dGa9bme0Zc4wqf9Socialr02YuZqRKwU3kBQtfRg4wIDAQAB";

  mine.dns.allowedNetworks = [
    "127.0.0.0/24"
    "178.197.128.0/17" # Swisscom
    "31.165.62.80" # Fritzbox
    "31.165.0.0/16" # Sunrise
    "195.176.96.0/19" # ETHZ
  ];

  mine.dns.ipnsHash = "QmcF3xqxFZxDLBJ5fNmr8vZ5p83SoS5zuavYMhizh2L1dp";

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
