{ pkgs, lib, config, ... }:

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
    server ${cfg.server.subnet} ${cfg.server.subnetMask}
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

      environment.etc = {
        "openvpn/clients/emma".text = ''
          ifconfig-push 10.149.76.3 ${cfg.server.subnetMask}
        '';
        "openvpn/clients/nepnep".text = ''
          ifconfig-push 10.149.76.2 ${cfg.server.subnetMask}
        '';
      };

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

      assertions = [{
        assertion = cfg.client.server.config.mine.openvpn.server.enable;
        message = ''
          The server ${cfg.client.server.config.networking.hostName} of the
          client ${config.networking.hostName} doesn't have the option
          mine.openvpn.server.enable set to true.
        '';
      }];

    })
  ];

}
