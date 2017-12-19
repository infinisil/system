{ nodes, config, pkgs, lib, ... }:

let

  cfg = config.localserver;

  domain = nodes.server.config.networking.domain;

in

with lib;

{

  options.localserver = {

    webserverport = mkOption {
      type = types.int;
      description = "which port to use for the webserver";
    };

    sshport = mkOption {
      type = types.int;
      description = "which port to use for ssh";
    };

  };

  config = {

    networking.firewall.allowedTCPPorts = [ 80 ];
    services.autossh.sessions = let
      common = ''-o "ServerAliveInterval 15" -o "ExitOnForwardFailure yes" -N ${domain}'';
    in [
      {
        name = "localserver";
        user = "root";
        extraArguments = ''-R ${toString cfg.webserverport}:localhost:80 '' + common;
      }
      {
        name = "ssh";
        user = "root";
        extraArguments = ''-R \*:${toString cfg.sshport}:localhost:22 '' + common;
      }
    ];

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
