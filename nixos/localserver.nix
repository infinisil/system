{ config, pkgs, ... }:
{
  services.autossh.sessions = [
    {
      name = "localserver";
      user = "root";
      extraArguments = ''-o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -N -R 81:localhost:8081 root@infinisil.io'';
    }
  ];


  services.nginx = {
    enable = true;
    virtualHosts."mac.infinisil.io" = {
      root = "/webroot";
      listen = [ { port = 8081; addr = "0.0.0.0"; } ];
    };
  };
}
