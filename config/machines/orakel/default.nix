{ lib, config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  mine.enableUser = true;

  services.openvpn.servers.orakel.mine = {
    type = "server";
    server.subnet = "10.99.1.0/24";
    server.staticClientIps =
      let clients = builtins.removeAttrs config.networking.connectivitySpec.vpn.orakel ["orakel"];
      in lib.mapAttrs' (client: lib.nameValuePair "orakel-${client}") clients;
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

  mine.deluged.enable = true;

  boot.loader.grub.enable = true;
  boot.loader.grub.splashImage = null;

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
        plan = "1d=>1h";
        destinations.vario = {
          host = config.networking.connections.vario;
          dataset = "tank2/root/torrent";
          plan = "1d=>1h";
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
          host = config.networking.connections.vario;
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
