{ config, pkgs, ... }:

let

  domain = (import <cfg/hosts>).dobby.networking.domain;

in

{

  networking.firewall.allowedTCPPorts = [ 80 ];
  services.autossh.sessions = let
    common = ''-o "ServerAliveInterval 15" -N ${domain}'';
  in [
    {
      name = "localserver";
      user = "root";
      extraArguments = ''-R 1808:localhost:80 '' + common;
    }
    {
      name = "ssh";
      user = "root";
      extraArguments = ''-R 223:localhost:22 '' + common;
    }
  ];

  users.extraUsers.infinisil.openssh.authorizedKeys.keys = [ (import ./keys.nix).server.infinisil ];

  users.extraUsers.root.openssh.authorizedKeys.keys = [ (import ./keys.nix).server.root ];

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
