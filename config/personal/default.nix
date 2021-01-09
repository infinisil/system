{ config, lib, pkgs, ... }: {

  imports = [
    ./znc.nix
    ./bins.nix
    ./user.nix
  ];

  time.timeZone = "Europe/Zurich";

  mine.webKeyDirectory.directory = ./wkd;

  security.acme.email = "acme@infinisil.com";
  security.acme.acceptTerms = true;

  mine.userConfig = {
    programs.gpg.enable = true;
    programs.gpg.settings.encrypt-to = "0x3EAC5A9F2DC4D47E";
    programs.gpg.settings.default-key = "0xE8F1E9EAD284E17D";
  };

  mine.keylayout = {
    enable = true;
    layoutFile = ./keymap.xkb;
    xcapeConfig."#94" = "Escape";
  };

  networking.extraHosts = ''
    192.168.178.1 fritz.box
  '';

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
      https://chrispenner.ca/atom.xml
    ];
  };

}
