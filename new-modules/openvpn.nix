{ lib, config, ... }:

with lib;

let

  cfg = config.mine.openvpn;

  serverIp = (head cfg.client.server.config.networking.interfaces.eth0.ipv4.addresses).address;
  port = 1194;

in

{

  options.mine.openvpn = {

    isServer = mkOption {
      type = types.bool;
      default = false;
      description = "whether this is the server. If true the client config will be ignored. If true the client config will be ignored.";
    };

    client = {
      enable = mkEnableOption "openvpn client";
      server = mkOption {
        type = types.unspecified;
        description = "server nixops node, needs to be set when client is enabled";
      };
    };

  };

  config = mkMerge [
    (mkIf cfg.isServer {

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

      environment.etc = {
        "openvpn/clients/emma".text = ''
          ifconfig-push 10.149.76.3 255.255.255.0
        '';
        "openvpn/clients/nepnep".text = ''
          ifconfig-push 10.149.76.2 255.255.255.0
        '';
      };

      services.openvpn.servers.server.config = ''
        port ${toString port}
        proto udp
        dev tun

        client-config-dir /etc/openvpn/clients

        ca /root/ca.crt
        cert /root/server.crt
        key /root/server.key
        dh /root/dh.pem

        topology subnet

        server 10.149.76.0 255.255.255.0

        push "redirect-gateway def1 bypass-dhcp"

        client-to-client

        keepalive 10 120

        tls-auth /root/ta.key 0

        cipher AES-256-CBC

        compress lz4-v2
        push "compress lz4-v2"

        user nobody
        group nobody

        persist-key
        persist-tun

        status /var/lib/openvpn/status.log

        verb 3
        mute 20

        explicit-exit-notify 1
      '';

    })
    (mkIf cfg.client.enable {

      services.openvpn.servers.server = {
        config = ''
          client

          dev tun

          proto udp

          remote ${serverIp} ${toString port}

          resolv-retry infinite

          nobind

          user nobody
          group nobody

          persist-key
          persist-tun

          ca /root/ca.crt
          cert /root/client.crt
          key /root/client.key

          remote-cert-tls server

          tls-auth /root/ta.key 1

          cipher AES-256-CBC

          verb 6

          mute 20
        '';
      };


      assertions = [{
        assertion = cfg.client.server.config.mine.openvpn.isServer;
        message = ''
          The server ${cfg.client.server.config.networking.hostName} of the
          client ${config.networking.hostName} doesn't have the option
          mine.openvpn.isServer set to true.
        '';
      }];

    })
  ];


}
