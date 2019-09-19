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

  mine.dns.dkimKey = "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDEdE2Wpenn1dVXmnRc/5eZu6uuxF+08ndXaG9QWhm9hBue1nvDOtayKyoJKG6NEfNQpGfSdtZ+MfXd8znBTz7SBbtZ4o/PVqjiF2zUVZcZ80YjGCxzKpI1PV6/Q5x6qkj7cEMX/X8pOnqBs05I4/bZ3xNUnj+Zm9OOK84iIoXipQIDAQAB";

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
