{

  imports = [
    ./keys.nix
    ./znc.nix
  ];

  mine.dns.dkimKey = "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDHt0p6SsJkq7m/r21RiWyweuFrYmqzL82+LMGD3XW6u6gjvE7sTZ7zhH+sxLNEd09mipzi2gRVYdKd7zyXDmWgskU/wvR4FBvKKTb7HWFGsZpYnhbP9cmfj7A1Ezg8SkF8QM63JDf0NMNfWBOZMBvCZYPgqde93b3cjQf4NkYKEwIDAQAB";

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
