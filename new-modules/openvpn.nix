{ lib, config, ... }:

with lib;

let

  cfg = config.mine.openvpn;

  serverIp = (head cfg.client.server.config.networking.interfaces.eth0.ipv4.addresses).address;
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

  server = ''
    server 10.149.76.0 255.255.255.0
    port ${toString port}

    topology subnet
    client-to-client
    push "redirect-gateway def1 bypass-dhcp"
    client-config-dir /etc/openvpn/clients
    compress lz4-v2
    push "compress lz4-v2"

    ca /root/ca.crt
    cert /root/server.crt
    key /root/server.key
    dh /root/dh.pem
    tls-auth /root/ta.key 0

    keepalive 10 120
    status /var/lib/openvpn/status.log
    explicit-exit-notify 1

    ${shared}
  '';

  client = ''
    client
    remote ${serverIp} ${toString port}
    nobind

    resolv-retry infinite
    remote-cert-tls server

    ca /root/ca.crt
    cert /root/client.crt
    key /root/client.key
    tls-auth /root/ta.key 1

    ${shared}
  '';

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

      services.openvpn.servers.server.config = server;

    })
    (mkIf cfg.client.enable {

      services.openvpn.servers.server = {
        config = client;
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
