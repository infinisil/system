{ lib, config, ... }:

with lib;

let

  cfg = config.mine.web;

  domain = config.networking.domain;

in

{

  options.mine.web = {
    enable = mkEnableOption "web server config";

    root = mkOption {
      type = types.path;
      description = "Webroot to use";
    };
  };

  config = mkIf cfg.enable {

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedProxySettings = true;

      virtualHosts."www.${domain}" = {
        globalRedirect = domain;
        forceSSL = true;
        enableACME = true;
      };
      virtualHosts."${domain}" = {
        forceSSL = true;
        enableACME = true;
        root = cfg.root;
      };
      virtualHosts."private.${domain}" = {
        forceSSL = true;
        enableACME = true;
        basicAuth.infinisil = config.private.passwords."infinisil@private.infinisil.com";
        root = "/privatewebroot";
      };
    };

  };


}
