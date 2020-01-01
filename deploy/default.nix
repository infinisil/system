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
}: import (import ../config/sources).nixoses {

  defaults = { name, lib, ... }: {
    enabled = if nodes == [] then true else lib.elem name nodes;
    host = if host-ips ? ${name} then "root@${host-ips.${name}}" else null;

    nixpkgs = ../external/nixpkgs;

    configuration = {
      imports = [
        ../config
        ../external/private
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
        ../external/private/machines/protos.nix
      ];
      system.stateVersion = "19.03";
    };
  };

  nodes.vario = {
    configuration = {
      imports = [
        ../config/machines/vario
        ../external/private/machines/vario.nix
        deployer
      ];
      system.stateVersion = "19.03";
    };
  };

  nodes.ninur = {
    configuration = {
      imports = [
        ../config/machines/ninur
        ../external/private/machines/ninur.nix
        deployer
      ];
      system.stateVersion = "19.03";
    };
  };

  nodes.orakel = {
    configuration = {
      imports = [
        ../config/machines/orakel
        ../external/private/machines/orakel.nix
      ];
      system.stateVersion = "19.03";
    };
  };

}
