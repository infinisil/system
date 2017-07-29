# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, fetchFromGitHub, ... }:

{
  #environment.etc."fail2ban/filter.d/bind.conf".source = pkgs.writeText "bind" ''
  #  [Definition]
  #  failregex = .*client @0x[0-9a-f]+ <HOST>#4444 \(cpsc\.gov\): query \(cache\).*
  #'';
  #environment.etc."fail2ban/action.d/blackhole.conf".source = pkgs.writeText "blackhole" ''
  #  [Definition]
  #  actionban = ip route add blackhole <ip>
  #  actionunban = ip route del blackhole <ip>
  #'';
  systemd.timers.block = {
    partOf = [ "block.service" ];
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "*:0/10:0"; # Every 10 minutes
  };
  systemd.services.block = {
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
  services.fail2ban = {
    enable = true;
    #  jails.bind = ''
    #    filter = bind
    #    action = blackhole
    #    enabled = false
    #    logpath = /var/log/named.log
    #    bantime = 3600
    #    findtime = 60
    #    maxretry = 5
    #  '';
  };

  services.bind = {
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

  networking.firewall.allowedTCPPorts = [ 53 ];
  networking.firewall.allowedUDPPorts = [ 53 ];
}
