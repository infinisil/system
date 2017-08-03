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
        ips=$(cat /var/log/named.log | grep 'cpsc.gov' | awk '{sub("#.*", "", $5);print $5;}' | sort | uniq)
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
          file = "/var/dns/infinisil.io";
          master = true;
          name = "infinisil.io";
        }
      ];
    };
  };
}
