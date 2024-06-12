{ config, pkgs, lib, sources, ... }:

with lib;

let

  cfg = config.mine.mail;

  module = import sources.nixos-mailserver;

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

    users.users.virtualMail = lib.mkForce {
      isSystemUser = true;
    };

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

      certificateScheme = "acme-nginx";

      enableImap = true;
      enablePop3 = true;
      enableImapSsl = true;
      enablePop3Ssl = true;

      enableManageSieve = true;

    };

    services.dovecot2.mailboxes = {
      Patreon.auto = "subscribe";
      Builds.auto = "subscribe";
      Services.auto = "subscribe";
      DMARC.auto = "subscribe";
      GitHub.auto = "subscribe";
    };

    services.dovecot2.sieveScripts.after2 = builtins.toFile "after2.sieve" ''
      require ["fileinto", "imap4flags"];
      if address :is :all ["to","cc"] "contact@infinisil.com" {
        keep;
      } elsif address :is :domain "from" "patreon.com" {
        fileinto "Patreon";
      } elsif address :is :all "cc" "ci_activity@noreply.github.com" {
        fileinto :flags "\\Seen" "Builds";
      } elsif address :is :all "to" "dmarc_rua@infinisil.com" {
        fileinto :flags "\\Seen" "DMARC";
      } elsif address :is :all "from" "notifications@github.com" {
        fileinto :flags "\\Seen" "GitHub";
      } elsif not address :is :domain ["to", "cc"] "infinisil.com" {
        fileinto "Junk";
        stop;
      } else {
        fileinto "Services";
      }
    '';
  };


}
