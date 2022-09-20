{ lib, config, pkgs, ... }:

with lib;

let

  mailAccounts = config.mailserver.loginAccounts;
  htpasswd = pkgs.writeText "radicale.users" (concatStrings
    (flip mapAttrsToList mailAccounts (mail: user:
      mail + ":" + user.hashedPassword + "\n"
    ))
  );

in

{

  options.mine.radicale.enable = mkEnableOption "radicale server";

  config = mkIf config.mine.radicale.enable {

    environment.systemPackages = with pkgs; [
      apacheHttpd
    ];

    networking.firewall.allowedTCPPorts = [ 5232 80 443 ];
    networking.firewall.allowedUDPPorts = [ 5232 ];

    services.nginx = {
      enable = true;
      virtualHosts."dav.${config.networking.domain}" = {
        enableACME = true;
        forceSSL = true;
        root = "/webroot/dav/";
        locations."/" = {
          proxyPass = "http://localhost:5232/";
          extraConfig = ''
            proxy_set_header  X-Script-Name /;
            proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_pass_header Authorization;
          '';
        };
      };
    };

    services.radicale = {
      enable = true;
      package = pkgs.radicale2;
      settings = {
        auth = {
          type = "htpasswd";
          htpasswd_filename = "htpasswd";
          htpasswd_encryption = "crypt";
        };
        storage = {
          filesystem_folder = "/var/lib/radicale/collections";
        };
        logging = {
          debug = true;
        };
      };
      # Hangs radicale:
      # storage.hook = ${pkgs.git}/bin/git add -A && (${pkgs.git}/bin/git diff --cached --quiet || ${pkgs.git}/bin/git commit -m 'Changes by infinisil')
    };

  };

}
