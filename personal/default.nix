{

  imports = [
    ./keys.nix
    ./znc.nix
  ];


  mine.dns.allowedNetworks = [
    "127.0.0.0/24"
    "178.197.128.0/17" # Swisscom
    "31.165.62.80" # Fritzbox
    "31.165.0.0/16" # Sunrise
    "195.176.96.0/19" # ETHZ
  ];

  mine.dns.ipnsHash = "QmcF3xqxFZxDLBJ5fNmr8vZ5p83SoS5zuavYMhizh2L1dp";

  mine.openvpn.server.subnet = "10.149.76.0";

}
