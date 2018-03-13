{ nodes, ... }: {

  imports = [
    ../hardware/mac
  ];

  mine.profiles.desktop.enable = true;

  mine.eth.sem6.enable = true;

  mine.live-wallpaper.enable = true;

  mine.rpf.client = {
    server = nodes.yuri;
    webport = 8021;
    sshport = 2021;
    subdomain = "mac";
  };

  nix.nixPath = [
    "nixpkgs=/cfg/nixpkgs"
  ];

  networking = {
    hostName = "emma";
    hostId = "34cc680d";
  };
}
