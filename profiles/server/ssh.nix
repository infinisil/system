{ config, pkgs, lib, ... }:

{

  mine.subdomains = [ "keys" ];

  services.nginx = {
    virtualHosts."keys.${config.networking.domain}" = {
      forceSSL = true;
      enableACME = true;
      root = "/webserver"; # Needed for ACME
      locations."/" = {
        root = config.lib.mine.attrToDerivation "keys" config.sshkeys;
        extraConfig = "autoindex on;";
      };
    };
  };

}
