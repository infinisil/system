{ lib, config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  mine.enableUser = true;

  networking.nat.enable = true;
  networking.nat.externalInterface = "eth0";
  networking.nat.internalInterfaces = [ "wg0" ];

  networking.firewall.allowedUDPPorts = [ 51820 ];

  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.99.2.1/24" ];
    listenPort = 51820;

    postSetup = ''
      ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.99.2.0/24 -o eth0 -j MASQUERADE
    '';

    # This undoes the above command
    postShutdown = ''
      ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.99.2.0/24 -o eth0 -j MASQUERADE
    '';

    peers = [
      {
        publicKey = lib.fileContents ../../../external/private/wireguard-keys/client-public;
        allowedIPs = [ "10.99.2.2/32" ];
      }
    ];
  };


  mine.transmission.enable = true;

  mine.profiles.server.enable = true;

  mine.music = {
    server = {
      enable = true;
      local = false;
      musicDir = "/home/infinisil/music";
      user = "infinisil";
      group = "users";
      password = config.private.passwords.mpd;
    };
  };

  mine.youtubeDl = {
    enable = true;
    user = "infinisil";
    mpdHost = "${config.private.passwords.mpd}@localhost";
  };

  boot.loader.grub.enable = true;
  boot.loader.grub.splashImage = null;
  boot.loader.grub.configurationLimit = 5;

  boot.kernelParams = [ "net.ifnames=0" ];

  boot.zfs.devNodes = "/dev";
  boot.loader.grub.device = "/dev/sda";

  networking = {
    hostName = "orakel";
    hostId = "cb8bdc78";
    defaultGateway = "51.15.187.1";
    interfaces.eth0.ipv4.addresses = [{
      address = "51.15.187.150";
      prefixLength = 20;
    }];
  };

  services.iperf3.enable = true;
  services.iperf3.openFirewall = true;

  services.znapzend = {
    enable = true;
    features.compressed = true;
    autoCreation = true;
    pure = true;
    zetup = {
      "tank/root/torrent/current" = {
        plan = "2h=>1h";
        destinations.vario = {
          host = "10.99.2.2";
          dataset = "tank2/root/torrent";
          plan = "2h=>1h";
        };
      };
      "tank/root/music" = {
        plan = "1d=>1h";
        #destinations.ninur = {
        #  host = config.networking.connections.ninur;
        #  dataset = "tank/music";
        #  plan = "1h=>5min,1d=>1h";
        #};
        destinations.vario = {
          host = "10.99.2.2";
          dataset = "tank2/root/music";
          plan = "1d=>1h";
        };
      };
    };
  };

  mine.hardware.cpuCount = 2;
  mine.hardware.swap = true;

  services.openssh.enable = true;

  system.stateVersion = "19.03";
}
