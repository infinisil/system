{ pkgs, lib, ... }:
let keys = import ./keys.nix; in

with import ../lib { inherit pkgs; };

with keys; {
  imports = [
    ./git-host.nix
    ./base.nix
  ];

  programs.ssh.startAgent = true;
  services.openssh.enable = true;

  services.nginx = {
    virtualHosts."keys.${config.domain}" = {
      forceSSL = true;
      enableACME = true;
      root = "/webserver"; # Needed for ACME
      locations."/" = {
        root = attrToDerivation "keys" keys;
        extraConfig = "autoindex on;";
      };
    };
  };

  users.users = {
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
    ];
  };
}
