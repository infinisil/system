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

    mine.subdomains = [ "www" ];

    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedProxySettings = true;

      virtualHosts."www.${domain}".globalRedirect = domain;
      virtualHosts."${domain}" = {
        forceSSL = true;
        enableACME = true;
        root = cfg.root;
      };
    };

  };


}
