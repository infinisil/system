{ lib, config, pkgs, ... }:
let
  cfg = config.mine.webKeyDirectory;
  domain = config.networking.domain;
  policyFile = pkgs.writeText "wks-policy" "";
in {

  options.mine.webKeyDirectory = {
    enable = lib.mkEnableOption "Web Key Directory";
    directory = lib.mkOption {
      type = lib.types.path;
      description = ''
        Which directory to use for Web Key Directory. See
        <xlink href="https://wiki.gnupg.org/WKDHosting"/> for generation.
      '';
    };
  };

  config = lib.mkIf cfg.enable {

    services.nginx = {
      enable = true;
      virtualHosts.${domain} = {
        locations."=/.well-known/openpgpkey/policy".alias = policyFile;
        locations."/.well-known/openpgpkey/".alias = "${cfg.directory + "/${domain}"}/";
      };
    };

  };
}
