{ config, lib, pkgs, ... }:

with lib;

let

  domain = config.networking.domain;
  ip = (head config.networking.interfaces.eth0.ipv4.addresses).address;
  ip6 = (head config.networking.interfaces.eth0.ipv6.addresses).address;


  timeType = types.str;

  records = {

    a = {
      ip = mkOption {
        type = ipv4Type;
      };
    };

    cname = {

    };

  };


  recordType = types.submodule ({ config, ... }: {
    options = {

      name = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Name, if null, inherit from previous record";
      };

      ttl = mkOption {
        type = types.nullOr timeType;
        default = null;
        description = "TTL, when null uses globally defined TTL of the zone file";
      };

      recordType = mkOption {
        type = types.str;
        description = "a or b";
      };

      recordData = mkOption {
        type = types.str;
        description = "data section";
      };
    };
  });

  recordsType = types.listOf recordType;

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

    records = mkOption {
      type = recordsType;
      default = [];
      description = "records";
    };

    dkimKey = mkOption {
      type = types.str;
      description = "DKIM key";
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
          $TTL 600
          $ORIGIN ${domain}.

          @ IN SOA ns1.${domain}. hostmaster.${domain}. (
            4
            3H
            15
            1w
            3h
          )

          ${domain}. IN MX 10 mail.${domain}.

          ${domain}. IN TXT "v=spf1 ip4:${ip} -all"
          _dmarc IN TXT "v=DMARC1; p=quarantine; rua=mailto:dmarc_rua@${domain}; ruf=mailto:dmarc_ruf@${domain}"

          mail._domainkey.${domain}. IN TXT "v=DKIM1; p=${config.mine.dns.dkimKey}"

          ${optionalString (config.mine.dns.ipnsHash != null) ''
            @ IN TXT "dnslink=/ipns/${config.mine.dns.ipnsHash}"
          ''}

          mail.infinisil.com. IN A ${ip}

          ${domain}. IN NS ns1.${domain}.
          ${domain}. IN NS ns2.${domain}.

          ns1 IN A ${ip}
          ns2 IN A ${ip}

          @ IN A ${ip}
          @ IN AAAA ${ip6}

          ${concatMapStringsSep "\n" (sub:
            "${sub} IN CNAME ${domain}."
          ) config.mine.subdomains}

          tune.infinisil.com. IN A 51.15.187.150
        '';
      }];
    };

  };

}
