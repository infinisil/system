{ config, nodes, ... }:

let

  module = fetchTarball {
    url = "https://github.com/Infinisil/nixbot/archive/0bc49bc28d9a9aaf2714eb8d619f8ce6ce572974.tar.gz";
    sha256 = "18mdn0cnimg70zpdd1lqs0s57bskyx6q31km4z1sychhqnvynwlj";
  } + "/module.nix";

in
{

  imports = [
    ../../hardware/do
    ./hardware-configuration.nix
    module
  ];

  mine.profiles.server.enable = true;

  services.nixbot.enable = true;

  services.ipfs = {
    enable = true;
    autostart = true;
    enableGateway = true;
    localDiscovery = false;
  };

  boot = {
    loader.grub.device = "/dev/vda";
    zfs.devNodes = "/dev";
    zfs.enableUnstable = true;
    kernelParams = [ "net.ifnames=0" ];
  };

  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC7zf2O8yBXxh2tX9v/3ZztXtYeV4W9vTY2iSrm92HSErjz5KcIY/AAKaqbWXHZgsZk2pehBqNbQMOwn0WWdLvil2+Ah97cvl7d9b9XdCkfOPhNB6FKcTzPmMp5Rivi/IodVMhT2xO9S1zO0Y2Q7dsYgk5leKyiD10pkcw23p6MPMKhKV2DPgY6BiszrTEVmtyOHpGkji9rE1iB9MyOINY9eC4etmnNINXMlwttV0GjbJI9WXXEQN2mRaPPp1PBWaPOgoP3ufKi9MR1hEhAantyrfBm2SeqjUvXG5JN1RyooohIWIHWXNJlYFldFPsCD/C1HnE5ylJeLBbZEw0TPb6x infinisil@NixOS"
  ];

  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    zetup = {
      "tank/root/data" = {
        plan = "1day=>1hour";
        recursive = true;
        destinations.vario = {
          host = "vario";
          dataset = "main/backup/servernew";
          plan = "1day=>1hour,1week=>1day,1month=>1week";
        };
      };
      "tank/root/music" = {
        plan = "1hour=>1hour";
        destinations.ninur = {
          host = "ninur";
          dataset = "tank/root/music";
          plan = "1hour=>1hour";
        };
      };
    };
  };

  services.nginx.virtualHosts."ninur.${config.networking.domain}" = {
    forceSSL = true;
    enableACME = true;
    root = "/webroot";
    locations."/".proxyPass = "http://10.149.76.3";
  };

  services.nginx.virtualHosts."vario.${config.networking.domain}" = {
    forceSSL = true;
    enableACME = true;
    root = "/webroot";
    locations."/".proxyPass = "http://10.149.76.2";
  };

  mine.subdomains = [ "vario" "ninur" ];

  networking = {
    hostName = "protos";
    hostId = "12345678";
    domain = "infinisil.com";
    defaultGateway = "104.248.128.1";
    defaultGateway6 = "2a03:b0c0:3:e0::1";
    interfaces.eth0 = {
      ipv4.addresses = [{
        address = "104.248.129.84";
        prefixLength = 20;
      }];
      ipv6.addresses = [{
        address = "2a03:b0c0:3:e0::96:6001";
        prefixLength = 64;
      }];
    };

    firewall.allowedTCPPorts = [ 12345 ];
  };
}
