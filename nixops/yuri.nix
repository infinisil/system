{ nodes, ... }: {

  imports = [
    ../hardware/yuri
  ];

  mine.profiles.server.enable = true;

  mine.rpf.server = {

    enable = true;
    clients = with nodes; [
      laptop
      pc
    ];

  };

  networking = {
    hostName = "yuri";
    hostId = "84efc532";
    domain = "infinisil.com";
    defaultGateway = "207.154.192.1";
    defaultGateway6 = "2a03:b0c0:3:d0::1";
    nameservers = [ "8.8.8.8" ];
    interfaces.eth0 = {
      ipv4.addresses = [{
        address = "207.154.198.134";
        prefixLength = 20;
      }];
      ipv6.addresses = [{
        address = "2a03:b0c0:3:d0::af7:3001";
        prefixLength = 64;
      }];
    };
  };
}
