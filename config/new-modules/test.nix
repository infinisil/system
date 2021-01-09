{ config, lib, ... }:

with lib;

let

  cfg = config.mine.test;

in

{

  options.mine.test = {

    subdomain = mkEnableOption "test subdomain and webserver";

    user = mkEnableOption "test user";

  };

  config = mkMerge [
    (mkIf cfg.subdomain {

      services.nginx.virtualHosts."test.${config.networking.domain}" = {
        forceSSL = true;
        enableACME = true;
        root = "/webroot/test";
        locations."/".extraConfig = "autoindex on;";
      };

    })
    (mkIf cfg.user {

      users.extraUsers.testuser = {
        description = "Test user";
        isNormalUser = true;
      };

    })
  ];

}
