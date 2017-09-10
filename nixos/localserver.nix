{ config, pkgs, ... }:
{

  networking.firewall.allowedTCPPorts = [ 80 ];
  services.autossh.sessions = [
    {
      name = "localserver";
      user = "root";
      extraArguments = ''-o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -N -R 81:localhost:80 root@infinisil.io'';
    }
    {
      name = "ssh";
      user = "root";
      extraArguments = ''-N -R 222:localhost:22 root@infinisil.io'';
    }
  ];

  users.extraUsers.infinisil.openssh.authorizedKeys.keys = [ (import ./keys.nix).server.infinisil ];

  users.extraUsers.root.openssh.authorizedKeys.keys = [ (import ./keys.nix).server.root ];

  services.openssh.enable = true;


  services.nginx = {
    enable = true;
    virtualHosts."localhost" = {
      root = "/webroot";
      listen = [ { port = 80; addr = "0.0.0.0"; } ];
    };
  };
}
