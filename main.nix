{ lib, config, ... }:

with lib;

let

  baseModules = nixpkgs: map (file: {
    _file = file;
    key = file;
    imports = [ (import file) ];
  }) (import (nixpkgs + "/nixos/modules/module-list.nix"));

  pkgsModule = nixpkgs: { config, ... }: rec {
    _file = ./main.nix;
    key = _file;
    config._module.args.pkgs = mkDefault (import nixpkgs {
      inherit (config.nixpkgs) localSystem config overlays crossSystem;
    });
  };

  nixosSubmodule = machines: { name, config, ... }: {

    options = {
      nixpkgs = mkOption {
        type = types.path;
        default = ./external/nixpkgs;
        description = "nixpkgs to use";
      };

      confi = mkOption {
        type = types.submodule' {
          args = {
            inherit name machines;
            baseModules = import (config.nixpkgs + "/nixos/modules/module-list.nix");
          };
        } (baseModules config.nixpkgs ++ [ (pkgsModule config.nixpkgs) ]);
        default = {};
        description = "NixOS config";
      };

    };

  };

in

{

  options = {

    machines = mkOption {
      type = types.attrsOf (types.submodule (nixosSubmodule config.machines));
      default = {};
    };

    all = mkOption {
      type = types.package;
    };
  };

  config = {

    all = (import <nixpkgs> {}).linkFarm "systems" (map (name: {
      inherit name;
      path = config.machines.${name}.confi.system.build.toplevel;
    }) (attrNames config.machines));

  };


}
