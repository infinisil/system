{ config, pkgs, lib, ... }:

{
  imports = [
    ./git-host.nix
  ];

  programs.ssh.startAgent = true;

  services.openssh = {
    enable = true;
    extraConfig = ''
      ClientAliveInterval 15
    '';
  };

  networking.firewall.allowedTCPPorts = [ 22 ];

  networking.subdomains = [ "keys" ];

  services.nginx = {
    virtualHosts."keys.${config.networking.domain}" = {
      forceSSL = true;
      enableACME = true;
      root = "/webserver"; # Needed for ACME
      locations."/" = {
        root = config.lib.mine.attrToDerivation "keys" config.sshkeys;
        extraConfig = "autoindex on;";
      };
    };
  };

  users.users = with config.sshkeys; {
    root.openssh.authorizedKeys.keys = [
      mac.nixos.root
    ];

    infinisil.openssh.authorizedKeys.keys = [
      mac.nixos.infinisil
      mac.osx.infinisil
    ];

    git.openssh.authorizedKeys.keys = [
      mac.nixos.infinisil
      server.infinisil
      mac.osx.infinisil
      iPhone.pass
      mac.osx.old
      pc.root
      pc.infinisil
    ];
  };
}
