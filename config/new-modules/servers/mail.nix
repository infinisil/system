{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.mail;

  module = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "infinisil";
    repo = "nixos-mailserver";
    rev = "fix-enable-conditions";
    sha256 = "1k8j5dn50pk6if7fgksh330bpsvxdv9s667amm1v3g8nx730vkcb";
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
