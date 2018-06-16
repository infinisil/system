{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.mail;

  module = import (builtins.fetchTarball {
    url = "https://github.com/r-raymond/nixos-mailserver/archive/ffc67fef469c0ec81c83b7d2681a8b2ca0c58849.tar.gz";
    sha256 = "1if8pshc4z5zn59zcnnjh55fk4gym0pjzlyxpc0x95l3vp916s40";
  });

  domain = config.networking.domain;

  mkMail = s: "${s}@${config.networking.domain}";

in

{

  imports = [ module ];

  options.mine.mail = {

    enable = mkEnableOption "Mail server config";

  };

  config = mkIf cfg.enable {

    mailserver = {

      enable = true;

      mailDirectory = "/var/lib/mail";
      dkimKeyDirectory = "/var/lib/dkim";

      fqdn = "mail.${domain}";
      domains = [ domain ];

      localDnsResolver = false;

      loginAccounts = {
        "${mkMail "contact"}" = {
          aliases = map mkMail [
            "admin"
            "info"
            "postmaster"
          ];
          catchAll = [ domain ];
        };
      };

      certificateScheme = 3;

      enableImap = true;
      enablePop3 = true;
      enableImapSsl = true;
      enablePop3Ssl = true;

      enableManageSieve = true;

    };
  };

}
