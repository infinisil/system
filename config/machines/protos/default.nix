{ config, ... }:

{

  imports = [
    ./hardware-configuration.nix
    ((import ../../sources).nixbot + "/module.nix")
  ];

  mine.enableUser = true;

  mine.saveSpace = true;

  mine.hardware = {
    cpuCount = 1;
    swap = true;
  };

  mine.profiles.server.enable = true;

  users.users.infinisil.extraGroups = [ "nixbot" ];
  users.users.nginx.extraGroups = [ "nixbot" ];

  services.murmur' = {
    enable = true;
    openFirewall = true;
    config.registerName = "Infinisil's Server";
    acmeDomain = "infinisil.com";
  };

  services.nixbot = {
    enable = true;
    channels = [ "nixos-unstable" "nixos-19.03" "nixos-18.09" ];
    config = {
      users = {
        commands.enable = true;
        nixrepl.enable = true;
        nixrepl.nixPath = [ "nixbotlib=/var/lib/nixbot/lib" ];
      };
      channels.nixos-unregistered.unreg.enable = true;
      channelDefaults = {
        pr.enable = true;
        commands.enable = true;
        nixrepl.enable = true;
        leaked.enable = true;
        karma.enable = true;
        nixrepl.nixPath = [ "nixbotlib=/var/lib/nixbot/lib" ];
        karma.blacklist = [ "c" ];
      };
    };

  };

  services.nginx.virtualHosts."nixbot.${config.networking.domain}" = {
    enableACME = true;
    forceSSL = true;
  };

  services.ipfs = {
    enable = true;
    autostart = true;
    enableGateway = true;
    localDiscovery = false;
  };

  boot = {
    loader.grub.device = "/dev/vda";
    zfs.devNodes = "/dev";
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
          host = config.networking.connections.vario;
          dataset = "main/backup/servernew";
          plan = "1day=>1hour,1week=>1day,1month=>1week";
        };
      };
    };
  };

  services.nginx.virtualHosts."ninur.${config.networking.domain}" = {
    forceSSL = true;
    enableACME = true;
    root = "/webroot";
    locations."/".proxyPass = "http://${config.networking.connections.ninur}";
  };

  services.nginx.virtualHosts."vario.${config.networking.domain}" = {
    forceSSL = true;
    enableACME = true;
    root = "/webroot";
    locations."/".proxyPass = "http://${config.networking.connections.vario}";
  };

  mine.subdomains = [ "vario" "ninur" "nixbot" ];

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

    firewall.allowedTCPPorts = [ 12345 1500 1501 ];
  };
}
