{ config, pkgs, lib, ... }: {

  imports = [
    ../../hardware/mac
    ./boot.nix
    ./hardware-configuration.nix
  ];
  hardware.opengl.driSupport32Bit = true;

  mine.sshMounts = lib.mapAttrs (name: value: {
    host = "infinisil@${value}:/home/infinisil";
    identity = "/home/infinisil/.ssh/id_rsa";
  }) config.networking.connections // {
    betty = {
      host = "infinisil@${config.networking.connections.vario}:/betty";
      identity = "/home/infinisil/.ssh/id_rsa";
    };
  };

  shivacam = {
    cam.enable = true;
    viewer.host = "ninur";
  };

  mine.openvpn.client = {
    enable = true;
    server = config.networking.connections.protos;
  };

  mine.gaming.enable = true;

  services.ipfs.enable = true;

  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    zetup."tank/root/data" = rec {
      plan = "15min=>5min,1h=>15min,1d=>1h,1w=>1d,1m=>1w";
      recursive = true;
      destinations.backup = {
        dataset = "main/backup/laptopnew";
        host = "192.168.178.28";
        inherit plan;
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

  services.nginx = {
    enable = true;
    virtualHosts.localhost = {
      root = "/webroot";
      listen = [ { port = 80; addr = "0.0.0.0"; } ];
    };
  };

  environment.systemPackages = [
    pkgs.steam
  ];

  networking = {
    hostName = "ninur";
    hostId = "34cc680d";
    firewall.allowedTCPPorts = [ 80 1500 1501 ];
  };
}
