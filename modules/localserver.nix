{ nodes, config, pkgs, lib, ... }:

let

  cfg = config.localserver;

  domain = nodes.server.config.networking.domain;

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

in

with lib;

{

  options.localserver = {

    webserverport = mkOption {
      type = types.ints.u16;
      description = "which port to use for the webserver";
    };

    sshport = mkOption {
      type = types.ints.u16;
      description = "which port to use for ssh";
    };

  };

  config = {

    networking.firewall.allowedTCPPorts = [ 80 ];

    systemd.services = {
      sshr = mkService "ssh" "\*:${toString cfg.sshport}:localhost:22";
      webr = mkService "webserver" "${toString cfg.webserverport}:localhost:80";
    };

    services.openssh = {
      enable = true;
      passwordAuthentication = false;
    };

    services.nginx = {
      enable = true;
      virtualHosts."localhost" = {
        root = "/webroot";
        listen = [ { port = 80; addr = "0.0.0.0"; } ];
      };
    };
  };
}
