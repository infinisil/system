{

  vpn.networks.orakel = {
    backend = "wireguard";
    subnet = "10.99.2.0/24";
    server = {
      node = "orakel";
      publicIp = "51.15.187.150";
      subnetIp = "10.99.2.1";
      internetGateway = true;
      internetGatewayInterface = "eth0";
      wireguard.publicKey = "t90gKxoMhKb1AscC1Ty8kfxPJQPzZ4tiaj0fn6NwLG4=";
      wireguard.privateKeyFile = "/root/wireguard-keys/private";
    };
    clients.vario = {
      subnetIp = "10.99.2.2";
      wireguard.publicKey = "F80/4J53cs1pwuLSGwKMtjLMzWYeQPy+QmUue+AJ2Co=";
      wireguard.privateKeyFile = "/root/wireguard-keys/private";
    };
  };

  vpn.networks.protos = {
    backend = "wireguard";
    subnet = "10.99.3.0/24";
    server = {
      node = "protos";
      publicIp = "206.81.23.189";
      subnetIp = "10.99.3.1";
      wireguard.publicKey = "N+kEAyKQBMbdyY+yRAEVOb1AbWFYveUIDDZ+ni5h4yI=";
      wireguard.privateKeyFile = "/root/wireguard-keys/private";
    };
    clients.vario = {
      subnetIp = "10.99.3.2";
      wireguard.publicKey = "F80/4J53cs1pwuLSGwKMtjLMzWYeQPy+QmUue+AJ2Co=";
      wireguard.privateKeyFile = "/root/wireguard-keys/private";
    };
  };

}
