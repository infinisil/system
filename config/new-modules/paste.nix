{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.mine.paste;

  subdomain = "paste";
  domain = "${subdomain}.${config.networking.domain}";

  pastebin = pkgs.writeScriptBin "pst" ''
    #!${pkgs.stdenv.shell}
    export PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.openssl ]}

    tmp=$(mktemp --suffix .paste)
    cp /dev/stdin $tmp

    # Use openssl to get binary sha256 output
    # Convert it to base64 (instead of hex, is shorter)
    # Convert to base64url <https://tools.ietf.org/html/rfc4648#section-5>
    # Only use last 10 bytes, excluding the padding "="

    id=$(openssl dgst -binary -sha256 $tmp | openssl base64 | tr +/ -_ | cut -b34-43)

    # This gives us log2(64)*10=60 bits of entropy,
    # bringing the chance of a collision to about 10E-12 for 6000 pastes
    # according to <https://en.wikipedia.org/wiki/Birthday_problem#Probability_table>

    mv $tmp "${cfg.dataDir}/$id"
    chmod a+r "${cfg.dataDir}/$id"

    echo -n https://${domain}/$id
  '';

in

{

  options.mine.paste = {
    enable = mkEnableOption "personal paste host";
    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/paste";
      description = "Directory to use for pastes";
    };
  };

  config = mkIf cfg.enable {

    system.activationScripts.pastedir = {
      text = ''
        mkdir -p "${cfg.dataDir}"
        chmod 0777 "${cfg.dataDir}"
      '';
      deps = [ "var" ];
    };

    environment.systemPackages = [
      pastebin
    ];

    mine.subdomains = [ subdomain ];

    services.nginx.virtualHosts."${domain}" = {
      forceSSL = true;
      enableACME = true;
      root = cfg.dataDir;
    };

  };
}
