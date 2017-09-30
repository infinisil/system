{ config, lib, pkgs, fetchFromGitHub, ... }:

with lib;

let
  domain = "infinisil.io";
  ip = "139.59.149.43";
  ip6 = "2a03:b0c0:3:d0::5df6:1";
  subdomains = [ "www" "dav" "keys" "test" "mail" "mac" ];
in
{
  networking.firewall = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  systemd = {
    timers.block = {
      partOf = [ "block.service" ];
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "*:0/10:0"; # Every 10 minutes
    };
    services.block = {
      enable = true;
      path = with pkgs; [ iproute gawk ];
      script = ''
        if [[ ! -f /var/log/named/named.log ]]; then
          exit 0
        fi

        ips=$(cat /var/log/named/named.log | grep 'cpsc.gov' | awk '{sub("#.*", "", $5);print $5;}' | sort | uniq)
        for ip in $ips; do
          ip route add blackhole $ip
          echo $ip >> /var/dns/blocked
        done
        echo "" > /var/log/named/named.log
      '';
    };
  };

  services = {
    fail2ban = {
      enable = true;
    };

    bind = {
      enable = true;
      cacheNetworks = [
        "127.0.0.0/24"
        "178.197.128.0/17" # Swisscom
        "31.165.62.80" # Fritzbox
      ];
      extraConfig = ''
        logging {
          channel security_file {
              file "/var/log/named/named.log" versions 3 size 30m;
              severity dynamic;
              print-time yes;
          };
          category security {
              security_file;
          };
        };
     '';
      zones = [
        {
          file = builtins.toFile domain ''
            $TTL 1d
            $ORIGIN ${domain}.

            @ IN SOA ns3.${domain}. hostmaster.${domain}. (
              2
              3H
              15
              1w
              3h
            )

            ${domain}. IN NS ns3.${domain}.
            ${domain}. IN NS ns4.${domain}.

            ns3 IN A ${ip}
            ns4 IN A ${ip}

            @ IN A ${ip}
            @ IN AAAA ${ip6}
            ${concatMapStringsSep "\n" (sub:
              "${sub} IN CNAME ${domain}."
            ) subdomains}
          '';

          master = true;
          name = domain;
        }
      ];
    };
  };
}
