{ config, lib, ... }:

with lib;

{

  options.mine.publicDir = {
    enable = mkEnableOption "Public directory";
  };

  config = mkIf config.mine.publicDir.enable {

    systemd.tmpfiles.rules = [ "d /var/lib/public 0777 root root -" ];

    mine.subdomains = [ "public" ];

    services.nginx = {
      enable = true;
      virtualHosts."public.${config.networking.domain}" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/lib/public";
        locations."/".extraConfig = "autoindex on;";
      };
    };
  };

}
