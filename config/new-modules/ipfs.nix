{ config, lib, ... }:

with lib;

let

  cfg = config.services.ipfs;

in

{

  options.services.ipfs = {
    enableGateway = mkEnableOption "ipfs gateway";
    gatewayIp = mkOption {
      type = types.ints.u16;
      default = 8080;
      description = "Port to use for gateway";
    };
    autostart = mkOption {
      type = types.bool;
      default = false;
      description = "Automatically start IPFS on startup";
    };
  };

  config = mkMerge [
    {
      systemd.services.ipfs.wantedBy = mkForce [];
      services.ipfs.gatewayAddress = "/ip4/127.0.0.1/tcp/${toString cfg.gatewayIp}";
    }
    (mkIf cfg.enable {
      users.users = mkMerge (map (user: {
        ${user}.extraGroups = [ cfg.group ];
      }) config.mine.mainUsers);
    })
    (mkIf (! cfg.autostart) {
    })
    (mkIf (cfg.enable && cfg.enableGateway) {
      mine.subdomains = [ "ipfs" ];

      services.nginx = {
        enable = true;
        recommendedProxySettings = mkForce false;
        virtualHosts."ipfs.${config.networking.domain}" = {
          forceSSL = true;
          enableACME = true;
          root = "/webroot";
          locations."/".proxyPass = "http://localhost:${toString cfg.gatewayIp}";
        };
      };

    })
  ];

}
