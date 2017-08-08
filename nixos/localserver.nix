{ config, pkgs, ... }:
{

  networking.firewall.allowedTCPPorts = [ 80 ];
  #services.autossh.sessions = [
  #  {
  #    name = "localserver";
  #    user = "root";
  #    extraArguments = ''-o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -N -R 81:localhost:80 root@infinisil.io'';
  #  }
  #];


  services.nginx = {
    enable = true;
    virtualHosts."localhost" = {
      root = "/webroot";
      listen = [ { port = 80; addr = "0.0.0.0"; } ];
    };
  };
}
