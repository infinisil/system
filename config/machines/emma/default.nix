{ nodes, ... }: {

  imports = [
    ../../hardware/mac
    ./boot.nix
    ./hardware-configuration.nix
  ];

  mine.openvpn.client = {
    enable = true;
    server = nodes.yuri;
  };

  mine.ipfs.enable = true;

  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    zetup."main/data" = rec {
      plan = "15min=>5min,1h=>15min,1d=>1h,1w=>1d,1m=>1w";
      recursive = true;
      destinations.backup = {
        dataset = "main/backup/laptop";
        host = "192.168.1.25";
        plan = plan + ",1y=>1m";
      };
    };
  };

  mine.string-transfer.enable = true;

  mine.profiles.desktop.enable = true;

  mine.eth.sem6.enable = true;

  mine.dev.rust.enable = true;

  hardware.opengl.enable = true;

  mine.server-sync = {
    enable = true;
    dataDir = "server/data";
    uploadDir = "server/upload";
    server = "infinisil.com";
  };

  mine.rpf.client = {
    server = nodes.yuri;
    webport = 8021;
    sshport = 2021;
    subdomain = "mac";
  };

  networking = {
    hostName = "emma";
    hostId = "34cc680d";
    firewall.allowedTCPPorts = [ 1500 1501 ];
  };
}
