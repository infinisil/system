{ lib, config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ((import ../../sources).nixbot + "/module.nix")
  ];

  services.on-demand-minecraft.enable = true;

  mine.mail.enable = true;
  mine.saveSpace = true;
  mine.radicale.enable = true;
  mine.paste.enable = true;
  mine.publicDir.enable = true;
  mine.gitHost.enable = true;
  mine.znc.enable = true;

  mine.dns.enable = true;

  mine.profiles.server.enable = true;

  mine.webKeyDirectory.enable = true;

  services.do-agent.enable = true;

  services.murmur = {
    enable = true;
    openFirewall = true;
    config.registerName = "Infinisil's Server";
    acmeDomain = "infinisil.com";
  };

  services.taskserver.enable = true;
  mine.web = {
    enable = true;
    root = "/webroot/www";
    keys.enable = true;
  };

  services.openvpn.servers.protos = {
    mine.type = "server";
    mine.server = {
      subnet = "10.99.0.0/24";
      staticClientIps =
        let clients = builtins.removeAttrs config.networking.connectivitySpec.vpn.protos ["protos"];
        in lib.mapAttrs' (client: lib.nameValuePair "protos-${client}") clients;
    };
  };
  services.openvpn.servers.orakel = {
    mine.type = "client";
    mine.client = {
      serverIp = config.networking.connections.orakel;
      makeDefaultGateway = false;
    };
  };

  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    zetup."tank/root/data" = {
      plan = "1d=>1h,1w=>1d";
      recursive = true;
      destinations.vario = {
        host = config.networking.connections.vario;
        dataset = "main/backup/protos";
        plan = "1d=>1h,1w=>1d,1m=>1w";
      };
    };
  };

  services.nixbot = {
    enable = true;
    channels = [ "nixos-unstable" "nixos-20.03" "nixos-19.09" "nixos-19.03" ];
    config = {
      users = {
        commands.enable = true;
        nixrepl.enable = true;
        nixrepl.nixPath = [ "nixbotlib=/var/lib/nixbot/lib" ];
      };
      channels.nixos-unregistered.unreg.enable = true;
      channels.home-manager.pr = {
        defaultRepo = "home-manager";
        defaultOwners.home-manager = "rycee";
      };
      channelDefaults = {
        quit.enable = true;
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

  mine.subdomains = [ "pc" "nixbot" ];

  services.nginx.virtualHosts."pc.infinisil.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://10.99.1.2:80";
  };

  users.users.infinisil.extraGroups = [ "nixbot" ];
  users.users.nginx.extraGroups = [ "nixbot" ];

  mine.enableUser = true;

  mine.hardware = {
    cpuCount = 1;
    swap = true;
  };

  boot = {
    loader.grub.device = "/dev/vda";
    zfs.devNodes = "/dev";
    kernelParams = [ "net.ifnames=0" ];
  };

  services.openssh.enable = true;

  networking = {
    domain = "infinisil.com";
    hostName = "protos";
    hostId = "6ad3ae1f";
    defaultGateway = "206.81.16.1";
    defaultGateway6 = "2a03:b0c0:3:d0::1";
    nameservers = [ "1.1.1.1" ];
    interfaces.eth0 = {
      ipv4.addresses = [{
        address = "206.81.23.189";
        prefixLength = 20;
      }];
      ipv6.addresses = [{
        address = "2a03:b0c0:3:d0::5f7f:5001";
        prefixLength = 64;
      }];
      macAddress = "ba:d5:84:08:05:c1";
    };
    interfaces.eth1 = {
      ipv4.addresses = [{
        address = "10.135.238.247";
        prefixLength = 16;
      }];
      macAddress = "4e:5c:97:f6:7e:bc";
    };
    firewall.allowedTCPPorts = [ 2362 ];
  };

  system.stateVersion = "19.03";

}
