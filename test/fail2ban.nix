{ config, lib, pkgs, fetchFromGitHub, ... }:

let
  logfile = "/var/log/named/named.log";
in

{
  environment.etc."fail2ban/filter.d/bind.conf".source = pkgs.writeText "bind" ''
    [Definition]
    failregex = .*client @0x[0-9a-f]+ <HOST>#4444 \(cpsc\.gov\): query \(cache\).*
  '';
  environment.etc."fail2ban/action.d/blackhole.conf".source = pkgs.writeText "blackhole" ''
    [Definition]
    actionban = ip route add blackhole <ip>
    actionunban = ip route del blackhole <ip>
  '';
  services.fail2ban = {
    enable = true;
    jails.bind = ''
      filter = bind
      action = blackhole
      enabled = false
      logpath = ${logfile}
      bantime = 3600
      findtime = 60
      maxretry = 5
    '';
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
            file "${logfile}" versions 3 size 30m;
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
