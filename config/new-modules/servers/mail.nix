{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.mail;

  module = import ((import <nixpkgs> {}).fetchFromGitLab {
    owner = "simple-nixos-mailserver";
    repo = "nixos-mailserver";
    rev = "c2ca4d1bb05a5c3886b433dc10b2c4d55bfa1f29";
    sha256 = "1l5xwk8rjla3bh1dqdf0j836h61a8piydcjhfm84n8hw7y13ajga";
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
