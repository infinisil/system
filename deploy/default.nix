let

  nixosConfig = builtins.toFile "configuration.nix" ''
    builtins.trace "This machine is managed by NixOps, using dummy configuration file for evaluation" {
      fileSystems."/".device = "/dev/sda1";
      boot.loader.grub.device = "nodev";

      assertions = [{
        assertion = false;
        message = "Not gonna do that for you";
      }];
    }
  '';

  deployer = { pkgs, ... }: {
    environment.shellAliases = {
      rb = toString ./rb;
      cachix-use = "cachix use -n -d ${toString ../config}";
    };
  };

  nurNoPkgs = import (import ../config/sources).nur {};

in

{ host-ips ? {}
, label ? ""
, nodes ? []
}: import /home/infinisil/prj/nixus {

  imports = [
    ../external/private
    ../config/multimods
  ];

  defaults = { name, lib, ... }: {
    enabled = (if nodes == [] then true else lib.elem name nodes) && host-ips ? ${name};
    host = if host-ips ? ${name} then host-ips.${name} else "${name}.invalid";
    hasFastConnection = host-ips ? ${name} && (lib.hasPrefix "192.168." host-ips.${name} || host-ips.${name} == "localhost");

    nixpkgs = ../external/nixpkgs;

    successTimeout = 120;
    switchTimeout = 240;

    configuration = {
      imports = [
        ../config
        ../external/private/default-old.nix
        nurNoPkgs.repos.rycee.modules.home-manager
      ];

      system.nixos.label = label;

      environment.etc.nixpkgs.source = lib.cleanSource (toString ../external/nixpkgs);

      nix.nixPath = [
        "nixos-config=${nixosConfig}"
        "nixpkgs=/etc/nixpkgs"
      ];
    };
  };

  nodes.protos = {
    configuration = {
      imports = [
        ../config/machines/protos
      ];
      system.stateVersion = "19.03";
    };
  };

  nodes.vario = {
    configuration = {
      imports = [
        ../config/machines/vario
        deployer
      ];
      system.stateVersion = "19.03";
    };
  };

  nodes.ninur = { lib, ... }: {
    enabled = lib.mkForce false;
    configuration = {
      imports = [
        ../config/machines/ninur
        deployer
      ];
      system.stateVersion = "19.03";
    };
  };

  nodes.orakel = {
    configuration = {
      imports = [
        ../config/machines/orakel
      ];
      system.stateVersion = "19.03";
    };
  };

}
