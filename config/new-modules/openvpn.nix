{ pkgs, lib, config, ... }:

let

  inherit (lib) types;
  ipLib = import ../lib/ip.nix lib;

  mySubmodule = { name, config, ... }: let

    cfg = config.mine;

  in {
    options = {
      # Export this openvpn configurations name for declaratively naming interfaces
      name = lib.mkOption {
        type = types.str;
        default = name;
      };

      mine.type = lib.mkOption {
        type = types.nullOr (types.enum [ "server" "client" ]);
        default = null;
        description = ''
          Which type of configuration this machine should use.
        '';
      };

      # Server-specific options
      mine.server = {
        subnet = lib.mkOption {
          type = types.strMatching "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+/[0-9]+";
          example = "10.176.56.0/24";
          description = ''
            Subnet this server should use. This should be a private IP range,
            see section 3 of <link
            xlink:href="https://tools.ietf.org/html/rfc1918">RFC 1918</link>.
          '';
        };

        port = lib.mkOption {
          type = types.port;
          default = 1194;
          description = ''
            UDP port to run the server on.
          '';
        };

        staticClientIps = lib.mkOption {
          type = types.attrsOf types.str;
          default = {};
          example = lib.literalExample ''{
            alpha = "10.176.56.45";
            beta = "10.176.56.2";
          }'';
          description = ''
            Assignment of static IP addresses for clients. The first available
            IP can't be used because it will be used by the server. So if
            <option>server.subnet</option> is <literal>10.176.56.0/24</literal>,
            the first available IP for clients would be
            <literal>10.176.56.2</literal>. The client name needs to correspond
            to the Common Name (CN) of the certificate. If the certificate
            was generated with <command>easyrsa build-client-full alpha
            </command>, the name will be <literal>alpha</literal>
          '';
        };
      };

      # Client-specific options
      mine.client = {
        serverIp = lib.mkOption {
          type = types.str;
          description = ''
            IP of the server to connect to.
          '';
        };

        serverPort = lib.mkOption {
          type = types.port;
          default = 1194;
          description = ''
            Port of the server to connect to.
          '';
        };

        makeDefaultGateway = lib.mkOption {
          type = types.bool;
          default = true;
          description = ''
            Whether to make this VPN connection be the default gateway, meaning
            by default all the clients internet traffic will go through the VPN
            server.
          '';
        };
      };

      # Public Key Infrastructure related options
      mine.pki = {
        caCertFile = lib.mkOption {
          type = types.path;
          description = ''
            Certificate for the CA that issued the server and clients
            certificates (non-secret). Should be the same for all clients
            and the server. Can be generated with
            <command>easyrsa build-ca</command>.
          '';
        };

        certFile = lib.mkOption {
          type = types.path;
          description = ''
            Certificate for this machine (non-secret). Every client should
            have a separate one, as should the server. These certificates
            need to be signed by the CA whose certificate was is specified
            with <option>caCertFile</option>. This certificate and
            the key for <option>keyFile</option> can be generated with
            <command>easyrsa build-server-full &lt;name&gt; nopass</command>
            for a server or
            <command>easyrsa build-client-full &lt;name&gt; nopass</command>
            .
          '';
        };

        keyFile = lib.mkOption {
          type = types.path;
          apply = toString;
          description = ''
            Secret private key for this machine. Every client should have a
            separate one, as should the server. This key needs to correspond
            to the certificate specified in <option>certFile</option>.
            This key and the certificate for <option>certFile</option> can
            be generated with
            <command>easyrsa build-server-full &lt;name&gt; nopass</command>
            for a server or
            <command>easyrsa build-client-full &lt;name&gt; nopass</command>
            .
          '';
        };

        dhParamFile = lib.mkOption {
          type = types.path;
          description = ''
            Diffie-Hellman parameters to be used (non-secret). Only needed
            for the server. Can be generated with
            <command>easyrsa gen-dh</command>.
          '';
        };

        tlsAuthFile = lib.mkOption {
          type = types.path;
          apply = toString;
          description = ''
            Secret shared TLS authentification key for the
            <literal>tls-auth</literal> field. Needs to be the same for
            the server and all clients. Can be generated with
            <command>openvpn --genkey --secret tls-auth.key</command>.
          '';
        };

      };
    };

    config = lib.mkIf (config.mine.type != null) {
      config = ''
        dev tun-${config.name}
        proto udp
        cipher AES-256-CBC

        persist-key
        persist-tun

        user nobody
        group nobody

        verb 3
        mute 20
      '' + {
        server = let

          parsedSubnet = ipLib.parseSubnet cfg.server.subnet;

          # Directory containing files for each client with a static IP
          clientConfig = pkgs.runCommandNoCC "openvpn-${name}-client-config" {} ''
            mkdir $out
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList (clientName: ip: ''
              echo "ifconfig-push ${ip} ${ipLib.prettyIp parsedSubnet.mask}" \
                > $out/${clientName}
            '') cfg.server.staticClientIps)}
          '';

        in ''
          server ${ipLib.prettyIp parsedSubnet.baseIp} ${ipLib.prettyIp parsedSubnet.mask}
          port ${toString cfg.server.port}

          topology subnet
          client-to-client
          client-config-dir ${clientConfig}
          compress lz4-v2
          push "compress lz4-v2"

          ca ${cfg.pki.caCertFile}
          cert ${cfg.pki.certFile}
          key ${cfg.pki.keyFile}
          dh ${cfg.pki.dhParamFile}
          tls-auth ${cfg.pki.tlsAuthFile} 0

          keepalive 10 120
          explicit-exit-notify 1
        '';
        client = ''
          client
          remote ${cfg.client.serverIp} ${toString cfg.client.serverPort}
          nobind

          ${lib.optionalString cfg.client.makeDefaultGateway
            "redirect-gateway def1 bypass-dhcp"}

          resolv-retry infinite
          remote-cert-tls server

          ca ${cfg.pki.caCertFile}
          cert ${cfg.pki.certFile}
          key ${cfg.pki.keyFile}
          tls-auth ${cfg.pki.tlsAuthFile} 1
        '';
      }.${cfg.type};

    };
  };

  serverConfigs = lib.filterAttrs (name: cfg: cfg.mine.type == "server") config.services.openvpn.servers;
  serverInterfaces = lib.mapAttrsToList (name: cfg: "tun-" + cfg.name) serverConfigs;

in

{

  options.services.openvpn.servers = lib.mkOption {
    type = types.attrsOf (types.submodule mySubmodule);
  };


  config = lib.mkIf (serverConfigs != []) {

    assertions =
      let portMapping = lib.zipAttrs (lib.mapAttrsToList (name: cfg: { ${toString cfg.mine.server.port} = name; }) serverConfigs);
      in lib.mapAttrsToList (port: names: {
        assertion = lib.length names < 2;
        message = "Multiple OpenVPN server instances are using the same port, namely "
          + "`services.openvpn.servers.{${lib.concatStringsSep "," names}}.server.port` "
          + "are all ${port}. Assign different ports to them instead.";
      }) portMapping
      ++
      lib.concatLists (lib.mapAttrsToList (name: cfg:
        let parsedSubnet = ipLib.parseSubnet cfg.mine.server.subnet;
        in lib.mapAttrsToList (clientName: clientIp: {
          assertion = parsedSubnet.check (ipLib.parseIp clientIp);
          message = "The IP ${clientIp} defined in `services.openvpn.servers.${name}.mine."
            + "server.staticClientIps.${clientName}` is not part of the servers "
            + "subnet ${cfg.mine.server.subnet} which goes from ${ipLib.prettyIp
            parsedSubnet.range.from} to ${ipLib.prettyIp parsedSubnet.range.to}.";
        }) cfg.mine.server.staticClientIps
      ) serverConfigs);

    networking = {
      firewall = {
        allowedUDPPorts = lib.mapAttrsToList (name: cfg: cfg.mine.server.port) serverConfigs;
        trustedInterfaces = serverInterfaces;
      };

      nat = {
        enable = true;
        internalInterfaces = serverInterfaces;
      };
    };

  };

}
