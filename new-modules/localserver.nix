{ config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.rpf;

  serverConfig = lib.mkMerge (lib.flip map cfg.server.clients (client: let
    # evaluate the clients configuration and get the rpf client config
    ccfg = client.config.mine.rpf.client;
  in lib.mkMerge [
    (lib.mkIf (ccfg.sshport != null) {

      services.openssh = {
        enable = true;
        gatewayPorts = "clientspecified";
        extraConfig = ''
          ClientAliveInterval 15
        '';
      };

      networking.firewall.allowedTCPPorts = lib.optional (ccfg.sshport != null) [ ccfg.sshport ];

    })
    (lib.mkIf (ccfg.webport != null && ccfg.subdomain != null) {

      services.nginx.virtualHosts."${ccfg.subdomain}.${config.networking.domain}" = {
        forceSSL = true;
        enableACME = true;
        root = "/webroot";
        locations."/".proxyPass = "http://localhost:${toString ccfg.webport}";
      };

      networking.subdomains = [ ccfg.subdomain ];

    })
  ]));

  mkService = desc: raction: {
    description = "remote ssh port forwarding for ${desc}";

    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      ExecStart = let
        args = lib.concatStringsSep " " [
          "-o \"ServerAliveInterval 15\""
          "-o \"ExitOnForwardFailure yes\""
          "-o \"ControlMaster no\""
          "-o \"ControlPersist no\""
          "-n -N"
          "-R ${raction}"
          domain
        ];
      in "${pkgs.openssh}/bin/ssh ${args}";
      Restart = "on-failure";
      RestartSec = 10;
    };
  };

  clientConfig = lib.mkMerge [
    (lib.mkIf (cfg.client.sshport != null) {

      services.openssh.enable = true;

      systemd.services.sshr = mkService "ssh"
        "\*:${toString cfg.client.sshport}:localhost:${toString (builtins.head config.services.openssh.ports)}";

    })
    (lib.mkIf (cfg.client.webport != null) {

      networking.firewall.allowedTCPPorts = [ 80 ];

      systemd.services.webr = mkService "webserver" "${toString cfg.client.webport}:localhost:80";

      services.nginx = {
        enable = true;
        virtualHosts."localhost" = {
          root = "/webroot";
          listen = [ { port = 80; addr = "0.0.0.0"; } ];
        };
      };

    })
  ];

in {
  options.mine.rpf = {

    server = {

      enable = lib.mkEnableOption "remote port forwarding server";

      clients = lib.mkOption {
        # list of nixos configurations, a la nixops' nodes argument
        type = types.listOf types.unspecified;
        default = [];
        description = "Clients allowed to use this server for remote port forwarding";
      };

    };

    client = {

      webport = lib.mkOption {
        type = types.nullOr types.ints.u16;
        description = "which port to use for the webserver";
        default = null;
      };

      sshport = lib.mkOption {
        type = types.nullOr types.ints.u16;
        description = "which port to use for ssh";
        default = null;
      };

      subdomain = lib.mkOption {
        type = types.nullOr types.str;
        description = "subdomain on the server to use for forwarding network traffic to this client";
        default = null;
      };

    };

  };

  config = lib.mkMerge [
    (lib.mkIf cfg.server.enable serverConfig)
    clientConfig
  ];

  #options = {
  #  enable = true;
  #  controlMaster = "auto";
  #  controlPersist = "60";
  #  matchBlocks = {
  #    inf = {
  #      hostname = "infinisil.com";
  #    };
  #    git = {
  #      hostname = "infinisil.com";
  #      user = "git";
  #    };
  #    laptop = {
  #      hostname = "infinisil.com";
  #      port = nodes.laptop.config.localserver.sshport;
  #    };
  #    pc = {
  #      hostname = "infinisil.com";
  #      port = nodes.pc.config.localserver.sshport;
  #    };
  #  };
  #};
}

