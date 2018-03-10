{ config, lib, pkgs, ... }:

with lib;

let

  domain = config.networking.domain;
  ip = (head config.networking.interfaces.eth0.ipv4.addresses).address;
  ip6 = (head config.networking.interfaces.eth0.ipv6.addresses).address;

in

{

  options.mine.dns = {

    enable = mkEnableOption "bind dns server";

    allowedNetworks = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Which networks are allowed to use this dns server";
    };

    ipnsHash = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "IPNS/IPFS hash to use for IPFS DNS lookups";
    };

  };


  config = mkIf config.mine.dns.enable {

    networking.firewall = {
      allowedTCPPorts = [ 53 ];
      allowedUDPPorts = [ 53 ];
    };

    environment.systemPackages = with pkgs; [
      bind
    ];

    services.bind = {
      enable = true;
      cacheNetworks = config.mine.dns.allowedNetworks;
      zones = [{
        name = domain;
        master = true;
        file = builtins.toFile domain ''
          $TTL 1d
          $ORIGIN ${domain}.

          @ IN SOA ns1.${domain}. hostmaster.${domain}. (
            3
            3H
            15
            1w
            3h
          )

          ${domain}. IN NS ns1.${domain}.
          ${domain}. IN NS ns2.${domain}.

          ns1 IN A ${ip}
          ns2 IN A ${ip}

          @ IN A ${ip}
          @ IN AAAA ${ip6}

          ${concatMapStringsSep "\n" (sub:
            "${sub} IN CNAME ${domain}."
          ) config.mine.subdomains}

          ${optionalString (config.mine.dns.ipnsHash != null) ''
            @ IN TXT "dnslink=/ipns/${config.mine.dns.ipnsHash}"
          ''}
        '';
      }];
    };

  };

}
