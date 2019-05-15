{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.mine.dns;

  domain = config.networking.domain;
  ip = (head config.networking.interfaces.eth0.ipv4.addresses).address;
  ip6 = (head config.networking.interfaces.eth0.ipv6.addresses).address;

  getRecords = zone: records: let
    serial = import (pkgs.runCommand "serial-${zone}" {
      records = records "0";
    } ''
      echo "\"$(( $(date +%s) / 60 ))\"" > $out
    '');
  in records serial;

in

{

  options.mine.dns = {

    enable = mkEnableOption "bind dns server";

    zones = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          master = mkOption {
            type = types.str;
          };

          slaves = mkOption {
            type = types.listOf types.str;
          };

          records = mkOption {
            type = types.unspecified; # function from serial number -> record lines, including SOA
          };
        };
      });
    };

  };


  config = mkIf config.mine.dns.enable {

    networking.firewall = {
      allowedTCPPorts = [ 53 ];
      allowedUDPPorts = [ 53 ];
    };

    environment.systemPackages = [ pkgs.bind ];

    services.bind = {
      enable = true;
      configFile = pkgs.writeText "named.conf" ''
        include "/etc/bind/rndc.key";
        controls {
          inet 127.0.0.1 allow {localhost;} keys {"rndc-key";};
        };

        options {
          directory "/run/named";
          pid-file "/run/named/named.pid";
        };

        ${concatMapStringsSep "\n\n" (zone:
        let
          value = cfg.zones.${zone};
          isMaster = config.networking.hostName == value.master;
          isSlave = elem config.networking.hostName value.slaves;
        in optionalString (isMaster || isSlave) ''
          zone "${zone}" {
            type ${if isMaster then "master" else "slave"};
            file ${pkgs.runCommand zone {
              nativeBuildInputs = [ pkgs.bind ];
              records = getRecords value.records;
              passAsFile = [ "records" ];
            } ''
              named-compilezone -o "$out" "${zone}" "$recordsPath"
            ''};
          }
        '') (attrNames cfg.zones)}
      '';
    };

  };

}
