{ config, lib, ... }:

with lib;

let

  cfg = config.mine.ipfs;

in

{


  options.mine.ipfs = {
    enable = mkEnableOption "ipfs config";

    enableGateway = mkEnableOption "ipfs gateway";

    autostart = mkOption {
      type = types.bool;
      default = false;
      description = "Automatically start IPFS on startup";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.ipfs = {
        enable = true;
        autoMount = false;
        dataDir = "/var/lib/ipfs";
      };

      users.users = mkMerge (map (user: {
        ${user}.extraGroups = [ "ipfs" ];
      }) config.mine.mainUsers);

      #networking.firewall.allowedTCPPorts = [ 4001 ];
    }
    (mkIf (! cfg.autostart) {
      systemd.services.ipfs.wantedBy = mkForce [];
    })
    (mkIf cfg.enableGateway {
      mine.subdomains = [ "ipfs" ];

      services.nginx = {
        enable = true;
        virtualHosts."ipfs.${config.networking.domain}" = {
          forceSSL = true;
          enableACME = true;
          root = "/webroot";
          locations."/".proxyPass = "http://localhost:8080";
        };
      };

    })
  ]);

}
