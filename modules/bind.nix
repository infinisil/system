{ config, lib, pkgs, ... }:

with lib;

let

  domain = config.networking.domain;
  ip = config.networking.interfaces.eth0.ipAddress;
  ip6 = config.networking.interfaces.eth0.ipv6Address;

in

{

  options = {
    networking.subdomains = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        List of subdomains to map to <literal>networking.domain</literal>
      '';
    };
  };

  config = {

    networking.firewall = {
      allowedTCPPorts = [ 53 ];
      allowedUDPPorts = [ 53 ];
    };

    environment.systemPackages = with pkgs; [
      bind
    ];

    services = {
      bind = {
        enable = true;
        cacheNetworks = [
          "127.0.0.0/24"
          "178.197.128.0/17" # Swisscom
          "31.165.62.80" # Fritzbox
          "195.176.96.0/19" # ETHZ
        ];
        zones = [
          {
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
              ${concatMapStringsSep "\n" (sub:
                "${sub} IN CNAME ${domain}."
              ) config.networking.subdomains}

              @ IN TXT "dnslink=/ipns/QmcF3xqxFZxDLBJ5fNmr8vZ5p83SoS5zuavYMhizh2L1dp"
            '';
          }
        ];
      };
    };

  };

}
