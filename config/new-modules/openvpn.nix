{ pkgs, lib, config, ... }:

with lib;

let

  cfg = config.mine.openvpn;

  port = 1194;

  shared = ''
    dev tun
    proto udp
    cipher AES-256-CBC

    persist-key
    persist-tun

    user nobody
    group nobody

    verb 3
    mute 20
  '';

  server = let
    client-config = pkgs.runCommand "openvpn-client-config" {} (''
      mkdir $out
    '' + concatMapStringsSep "\n" (name: ''
      echo "ifconfig-push ${cfg.server.client.${name}} ${cfg.server.subnetMask}" > $out/${name}
    '') (attrNames cfg.server.clients));
  in ''
    server ${cfg.server.subnet} ${cfg.server.subnetMask}
    port ${toString port}

    topology subnet
    client-to-client
    push "redirect-gateway def1 bypass-dhcp"
    client-config-dir ${client-config}
    compress lz4-v2
    push "compress lz4-v2"

    ca ${config.private.openvpn.ca}
    cert ${config.private.openvpn.cert}
    key ${config.private.openvpn.key}
    dh ${config.private.openvpn.dh}
    tls-auth ${config.private.openvpn.tls-auth} 0

    keepalive 10 120
    status /var/lib/openvpn/status.log
    explicit-exit-notify 1

    ${shared}
  '';

  client = ''
    client
    remote ${cfg.client.server} ${toString port}
    nobind

    resolv-retry infinite
    remote-cert-tls server

    ca ${config.private.openvpn.ca}
    cert ${config.private.openvpn.cert}
    key ${config.private.openvpn.key}
    tls-auth ${config.private.openvpn.tls-auth} 1

    ${shared}
  '';

in

{

  options.mine.openvpn = {

    server = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "whether this is the server. If true the client config will be ignored. If true the client config will be ignored.";
      };

      subnet = mkOption {
        type = types.str;
        description = "Private subnet to use";
      };

      subnetMask = mkOption {
        type = types.str;
        default = "255.255.255.0";
        description = "Subnet mask to use";
      };

      fixedClients = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "Mapping from names to openvpn ip's that are fixed";
      };
    };

    client = {
      enable = mkEnableOption "openvpn client";
      server = mkOption {
        type = types.str;
        description = "server ip";
      };
    };

  };

  config = mkMerge [
    (mkIf cfg.server.enable {

      networking = {
        firewall = {
          trustedInterfaces = [ "tun0" ];
          allowedUDPPorts = [ port ];
        };

        nat = {
          enable = true;
          externalInterface = "eth0";
          internalInterfaces = [ "tun0" ];
        };
      };

      systemd.services.openvpn-server.preStart = "mkdir -p /var/lib/openvpn";

      services.openvpn.servers.server.config = server;

    })
    (mkIf cfg.client.enable {

      systemd.services.openvpn-server-start = {
        description = "OpenVPN server restart after suspend script";
        wantedBy = [ "suspend.target" ];
        after = [ "suspend.target" ];
        serviceConfig.ExecStart = "${pkgs.systemd}/bin/systemctl --no-block restart openvpn-server";
      };

      services.openvpn.servers.server.config = client;

    })
  ];

}
