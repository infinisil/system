{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.mail;

  module = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "r-raymond";
    repo = "nixos-mailserver";
    rev = "v2.1.3";
    sha256 = "12wbz6shz00n7nv6j8x86q7n0q0d8vvc3b2psda2hvhywxvhhf3m";
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
