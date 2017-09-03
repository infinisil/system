{ config, lib, pkgs, fetchFromGitHub, ... }:

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
        "178.197.128.0/17"
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
          file = builtins.toFile "infinisil.io" ''
            $ORIGIN infinisil.io.
            $TTL        86400

            @ 1D IN SOA ns3.infinisil.io. hostmaster.infinisil.io. (
              1
              3H
              15
              1w
              3h
            )

            infinisil.io. IN NS ns3.infinisil.io.
            infinisil.io. IN NS ns4.infinisil.io.

            ns3 IN A 139.59.149.43
            ns4 IN A 139.59.149.43
            www IN CNAME infinisil.io.
            dav IN CNAME infinisil.io.
            keys IN CNAME infinisil.io.
            test IN CNAME infinisil.io.

          '';

          master = true;
          name = "infinisil.io";
        }
      ];
    };
  };
}
