{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.mail;

  module = import ((import <nixpkgs> {}).fetchFromGitLab {
    owner = "simple-nixos-mailserver";
    repo = "nixos-mailserver";
    rev = "817d84d36d34616ecf1a6ed6cba4fb1327b3a74f";
    sha256 = "0f7j61nlh4f3nqq3hbw0k4aq4mnmlp12cmkvyfwzrai92lpza4f9";
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
