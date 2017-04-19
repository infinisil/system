{ ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "2001:4860:4860::8844"
      "2001:4860:4860::8888"
      "8.8.8.8"
    ];
    defaultGateway = "139.59.144.1";
    defaultGateway6 = "";
    interfaces = {
      eth0 = {
        ip4 = [
          { address="139.59.149.239"; prefixLength=20; }
{ address="10.19.0.5"; prefixLength=16; }
        ];
        ip6 = [
          { address="2a03:b0c0:3:d0::42d4:a001"; prefixLength=64; }
{ address="fe80::1480:7aff:fe80:d2ad"; prefixLength=64; }
        ];
      };
      
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="16:80:7a:80:d2:ad", NAME="eth0"
    ATTR{address}=="36:10:3f:7d:e6:e4", NAME="eth0"
  '';
}
