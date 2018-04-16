{ config, pkgs, lib, mylib, ... }:

with lib;

{

  options.mine.web.keys.enable = mkEnableOption "Online SSH keys";

  config = mkIf config.mine.web.keys.enable {
    mine.subdomains = [ "keys" ];

    services.nginx = {
      enable = true;
      virtualHosts."keys.${config.networking.domain}" = {
        forceSSL = true;
        enableACME = true;
        root = "/webserver"; # Needed for ACME
        locations."/" = {
          root = mylib.attrToDerivation {
            name = "keys";
            value = config.mine.sshkeys;
          };
          extraConfig = "autoindex on;";
        };
      };
    };
  };

}
