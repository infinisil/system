{ pkgs, lib, ... }:
let
  keys = import ./keys.nix;

  valToPath = name: value: if builtins.isAttrs value then ''
    mkdir "${name}" && pushd "${name}"
    ${lib.concatMapStringsSep "\n" (attrName: valToPath attrName value.${attrName}) (builtins.attrNames value)}
    popd
  '' else ''
    echo "${toString value}" > "${name}"
  '';
  
  attrToDerivation = name: attrs: pkgs.runCommand name {} (valToPath "$out" attrs);
in
  
with keys; {
  imports = [
    ./git-host.nix
    ./base.nix
  ];

  programs.ssh.startAgent = true;
  services.openssh.enable = true;
  
  services.nginx = {
    virtualHosts."keys.infinisil.io" = {
      #forceSSL = true;
      #enableACME = true;
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
