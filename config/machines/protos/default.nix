{ config, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  mine.mail.enable = true;
  mine.saveSpace = true;
  mine.radicale.enable = true;
  mine.paste.enable = true;
  mine.publicDir.enable = true;
  mine.gitHost.enable = true;

  mine.profiles.default.enable = true;
  mine.profiles.server.enable = true;

  mine.webKeyDirectory.enable = true;

  services.do-agent.enable = true;

  services.taskserver.enable = true;
  mine.web = {
    enable = true;
    root = "/webroot/www";
    keys.enable = true;
  };

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
    firewall.allowedTCPPorts = [ 2362 ];
    firewall.allowedUDPPorts = [ 51820 ];
  };

}
