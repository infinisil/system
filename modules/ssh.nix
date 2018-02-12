{ config, pkgs, lib, ... }:

{

  programs.ssh.startAgent = true;

  services.openssh = {
    enable = true;
    forwardX11 = true;
    gatewayPorts = "clientspecified";
    extraConfig = ''
      ClientAliveInterval 15
    '';
  };

  networking.firewall.allowedTCPPorts = [ 22 ];


  users.users.infinisil.openssh.authorizedKeys.keys = config.allsshkeys;
  users.users.root.openssh.authorizedKeys.keys = config.allsshkeys;

}
