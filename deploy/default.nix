{ label, host-ips }:

let

  nixosConfig = builtins.toFile "configuration.nix" ''
    builtins.trace "This machine is managed by NixOps, using dummy configuration file for evaluation" {
      fileSystems."/".device = "/dev/sda1";
      boot.loader.grub.device = "nodev";

      assertions = [{
        assertion = false;
        message = "Not gonna do that for you, this is a nixops managed machine";
      }];
    }
  '';

  deployer = { pkgs, ... }: {
    environment.systemPackages = [ pkgs.nixopsUnstable ];

    environment.shellAliases = {
      rb = toString ./rb;
      cachix-use = "cachix use -n -d ${toString ../config}";
    };

    environment.variables = {
      NIXOPS_STATE = toString ../external/private/deployments.nixops;
      NIXOPS_DEPLOYMENT = "infinisil";
    };
  };

  nurNoPkgs = import (import ../config/sources).nur {};

in
{
  network = {
    description = "Infinisil's machines";
    enableRollback = true;
  };

  defaults = { name, lib, ... }: {
    deployment.targetHost = host-ips.${name} or "${name}.invalid";
    system.nixos.label = label;
    imports = [
      ../config
      ../external/private
      nurNoPkgs.repos.rycee.modules.home-manager
    ];

    environment.etc.nixpkgs.source = lib.cleanSource (toString ../external/nixpkgs);

    nix.nixPath = [
      "nixos-config=${nixosConfig}"
      "nixpkgs=/etc/nixpkgs"
    ];
  };

  protos = {
    imports = [
      ../config/machines/protos
      ../external/private/machines/protos.nix
    ];
  };

  ninur = {
    deployment.hasFastConnection = true;
    imports = [
      ../config/machines/ninur
      ../external/private/machines/ninur.nix
      deployer
    ];
  };

  vario = {
    imports = [
      ../config/machines/vario
      ../external/private/machines/vario.nix
      deployer
    ];
  };

  orakel = {
    imports = [
      ../config/machines/orakel
      ../external/private/machines/orakel.nix
    ];
  };
}
