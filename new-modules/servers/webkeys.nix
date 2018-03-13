{ config, pkgs, lib, ... }:

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
          root = config.lib.mine.attrToDerivation "keys" config.mine.sshkeys;
          extraConfig = "autoindex on;";
        };
      };
    };
  };

}
