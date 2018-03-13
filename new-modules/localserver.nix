{ nodes, config, pkgs, lib, ... }:

with lib;

let

  cfg = config.mine.rpf;

  serverConfig = {

    services.openssh = mkIf (any (c: c.config.mine.rpf.client.sshport != null) cfg.server.clients) {
      enable = true;
      gatewayPorts = mkDefault "clientspecified";
      extraConfig = ''
        ClientAliveInterval 15
      '';
    };

    networking.firewall.allowedTCPPorts = filter (p: p != null) (map (c: c.config.mine.rpf.client.sshport) cfg.server.clients);

    mine.subdomains = filter (p: p != null) (map (c: c.config.mine.rpf.client.subdomain) cfg.server.clients);

    services.nginx.virtualHosts = mkMerge (map (c: {

      "${c.subdomain}.${config.networking.domain}" = {
        forceSSL = true;
        enableACME = true;
        root = "/webroot";
        locations."/".proxyPass = "http://localhost:${toString c.webport}";
      };

    }) (filter (c: c.subdomain != null && c.webport != null) (map (c: c.config.mine.rpf.client) cfg.server.clients)));
  };

  mkService = desc: domain: raction: {
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

  clientConfig = {

    services.openssh.enable = mkIf (cfg.client.sshport != null) true;

    systemd.services.sshr = mkIf (cfg.client.sshport != null)
      (mkService "ssh" cfg.client.server.config.networking.domain
      "\*:${toString cfg.client.sshport}:localhost:${toString (builtins.head config.services.openssh.ports)}");

    networking.firewall.allowedTCPPorts = mkIf (cfg.client.webport != null) [ 80 ];

    systemd.services.webr = mkIf (cfg.client.webport != null)
      (mkService "webserver" cfg.client.server.config.networking.domain
      "${toString cfg.client.webport}:localhost:80");

    services.nginx = mkIf (cfg.client.webport != null) {
      enable = true;
      virtualHosts.localhost = {
        root = "/webroot";
        listen = [ { port = 80; addr = "0.0.0.0"; } ];
      };
    };

  };

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
      server = lib.mkOption {
        type = types.nullOr types.unspecified;
        description = "server to use for remote port forwarding";
        default = null;
      };

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

