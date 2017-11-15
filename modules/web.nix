{ config, ... }:

let

  domain = config.networking.domain;

in

{

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  networking.subdomains = [ "www" ];

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
      root = "/webroot/www";
    };
  };

}
