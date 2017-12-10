{ nodes, config, pkgs, ... }:

let

  domain = nodes.server.config.networking.domain;

in

{

  networking.firewall.allowedTCPPorts = [ 80 ];
  services.autossh.sessions = let
    common = ''-o "ServerAliveInterval 15" -o "ExitOnForwardFailure yes" -N ${domain}'';
  in [
    {
      name = "localserver";
      user = "root";
      extraArguments = ''-R 1809:localhost:80 '' + common;
    }
    {
      name = "ssh";
      user = "root";
      extraArguments = ''-R 2221:localhost:22 '' + common;
    }
  ];

  users.extraUsers.infinisil.openssh.authorizedKeys.keys = [
    config.sshkeys.server.infinisil
  ];

  users.extraUsers.root.openssh.authorizedKeys.keys = [
    config.sshkeys.server.root
  ];

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."localhost" = {
      root = "/webroot";
      listen = [ { port = 80; addr = "0.0.0.0"; } ];
    };
  };
}
