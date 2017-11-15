{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    samba
  ];

  networking.firewall = {
    allowedTCPPorts = [ 139 445 ];
    allowedUDPPorts = [ 137 138 ];
  };

  services.samba = {
    enable = true;
    shares = {
      root = {
        path = "/";
        "read only" = false;
      };
      betty = {
        path = "/betty";
        "read only" = false;
      };
    };
  };
}
