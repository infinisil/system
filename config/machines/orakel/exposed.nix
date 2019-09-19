evalHost: {
  publicIp = "51.15.187.150";


  openvpn.server = {
    subnet = "10.99.1.0/24";
    staticClientIps = {
      orakel = "10.99.1.1";
      vario = "10.99.1.2";
      protos = "10.99.1.3";
      ninur = "10.99.1.4";
    };
  };
}
