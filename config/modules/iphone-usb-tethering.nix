{ lib, config, ... }: {
  options.networking.iphoneUsbTethering.enable = lib.mkEnableOption "iPhone USB tethering";

  config = lib.mkIf config.networking.iphoneUsbTethering.enable {

    # Needed for iPhone pairing
    services.usbmuxd.enable = true;

    # Predictably names the iphone's tethering interface "iphone"
    #systemd.network.links."81-iphone" = {
    #  matchConfig.Driver = "ipheth";
    #  linkConfig.Name = "iphone";
    #  networkConfig.DHCP = true;
    #};

    # Enable DHCP to set up correct routes
    #networking.interfaces.iphone.useDHCP = true;
  };
}
