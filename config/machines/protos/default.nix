{ lib, config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ((import ../../sources).nixbot + "/module.nix")
  ];


  services.zrepl = {
    enable = true;
    settings = {
      jobs = [
        {
          type = "source";
          name = "data";
          serve = {
            type = "tcp";
            listen = ":8888";
            clients = {
              "10.99.3.2" = "vario";
            };
          };
          filesystems."tank/root/data<" = true;
          snapshotting = {
            type = "periodic";
            interval = "1h";
            prefix = "zrepl_";
          };
          send.raw = true;
        }
      ];
    };
  };


  secrets.files.syncplay.file = ../../../external/private/secrets/syncplay;
  secrets.files.syncplay.user = "syncplay";

  services.syncplay = {
    enable = true;
    certDir =
      let base = config.security.acme.certs."infinisil.com".directory;
      in pkgs.runCommandNoCC "syncplay-certs" {} ''
        mkdir $out
        ln -s ${base}/cert.pem $out/cert.pem
        ln -s ${base}/key.pem $out/privkey.pem
        ln -s ${base}/chain.pem $out/chain.pem
      '';
    salt = "WQCMMEFEPA";
    passwordFile = config.secrets.files.syncplay.file;
    user = "syncplay";
    group = config.security.acme.certs."infinisil.com".group;
  };

  users.users.syncplay = {
    isSystemUser = true;
  };

  users.users.nixbot.isSystemUser = true;

  mine.mail.enable = true;
  mine.saveSpace = true;
  mine.radicale.enable = true;
  mine.paste.enable = true;
  mine.publicDir.enable = true;
  mine.gitHost.enable = true;

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

  services.nixbot = {
    enable = true;
    channels = [ "nixos-unstable" "nixos-20.03" "nixos-19.09" "nixos-19.03" ];
    config = {
      host = "devoted-teal-duck.rmq.cloudamqp.com";
      port = 5671;
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
      channels.nixos-borg.pr = {
        defaultRepo = "ofborg";
      };
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
    useDHCP = false;
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
    firewall.allowedTCPPorts = [ 2362 config.services.syncplay.port ];
    firewall.allowedUDPPorts = [ 51820 ];
  };

  system.stateVersion = "19.03";

}
