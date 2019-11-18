{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.mail;

  module = import (import ../../sources).nixos-mailserver;

  domain = config.networking.domain;

  mkMail = s: "${s}@${config.networking.domain}";

in

{

  imports = [ module ];

  options.mine.mail = {

    enable = mkEnableOption "Mail server config";

  };

  config = mkIf cfg.enable {

    assertions = [{
      assertion = pkgs.python3.pkgs.pyspf.version != "2.0.13";
      message = "pyspf version 2.0.13 shouldn't be used, see https://github.com/NixOS/nixpkgs/pull/73427";
    }];

    services.rspamd.workers.rspamd_proxy.type = mkForce "rspamd_proxy";

    mailserver = {

      enable = true;

      mailDirectory = "/var/lib/mail";
      dkimKeyDirectory = "/var/lib/dkim";

      fqdn = "mail.${domain}";
      domains = [ domain ];

      localDnsResolver = false;

      loginAccounts = {
        "${mkMail "contact"}" = {
          aliases = [ (mkMail "") ];
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
