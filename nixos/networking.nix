{ ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "2001:4860:4860::8844"
      "2001:4860:4860::8888"
      "8.8.8.8"
    ];
    defaultGateway = {
      address = "138.68.64.1";
      interface = "eth0";
    };
    defaultGateway6 = {
      address = "2a03:b0c0:3:d0::1";
      interface = "eth0";
    };
    interfaces = {
      eth0 = {
        ip4 = [
          { address="138.68.69.164"; prefixLength=20; }
{ address="10.19.0.6"; prefixLength=16; }
        ];
        ip6 = [
          { address="2a03:b0c0:3:d0::4642:d001"; prefixLength=64; }
{ address="fe80::905e:2fff:fe4a:3676"; prefixLength=64; }
        ];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="92:5e:2f:4a:36:76", NAME="eth0"
  '';
}
