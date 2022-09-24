{ lib, config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  mine.remoteMusic.enable = true;

  #services.music-server.enable = true;

  services.murmur = {
    enable = true;
    openFirewall = true;
    config.registerName = "Infinisil's Other Server";
    acmeDomain = "torrent.infinisil.com";
  };

  secrets.files.syncplay.file = ../../../external/private/secrets/syncplay;
  secrets.files.syncplay.user = "syncplay";

  services.syncplay = {
    enable = true;
    certDir =
      let base = config.security.acme.certs."torrent.infinisil.com".directory;
      in pkgs.runCommandNoCC "syncplay-certs" {} ''
        mkdir $out
        ln -s ${base}/cert.pem $out/cert.pem
        ln -s ${base}/key.pem $out/privkey.pem
        ln -s ${base}/chain.pem $out/chain.pem
      '';
    salt = "WQCMMEFEPA";
    passwordFile = config.secrets.files.syncplay.file;
    user = "syncplay";
    group = config.security.acme.certs."torrent.infinisil.com".group;
  };

  users.users.syncplay = {
    isSystemUser = true;
    group = "syncplay";
  };

  users.groups.syncplay = {};

  users.users.nginx.extraGroups = [ "users" ];

  mine.enableUser = true;

  mine.transmission.enable = true;

  mine.profiles.default.enable = true;
  mine.profiles.server.enable = true;

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
    firewall.allowedTCPPorts = [ config.services.syncplay.port ];
  };

  services.iperf3.enable = true;
  services.iperf3.openFirewall = true;

  mine.hardware.cpuCount = 2;
  mine.hardware.swap = true;

  services.openssh.enable = true;
}
