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

{ nodes ? []
, ignoreFailingSystemdUnits ? true
}: import /home/infinisil/prj/nixus {
  libOverlay = self: super: {
    ip = import ./lib/ip.nix self;
  };
} {

  imports = [
    ../external/private
    ../config/multimods
  ];

  defaults = { name, lib, ... }: {
    enabled = if nodes == [] then true else lib.elem name nodes;

    nixpkgs = ../external/nixpkgs;

    switchTimeout = 240;

    configuration = {
      imports = [
        ../config
        ../external/private/default-old.nix
        nurNoPkgs.repos.rycee.modules.home-manager
      ];

      environment.etc.nixpkgs.source = lib.cleanSource (toString ../external/nixpkgs);

      nix.nixPath = [
        "nixos-config=${nixosConfig}"
        "nixpkgs=/etc/nixpkgs"
      ];
    };

    inherit ignoreFailingSystemdUnits;
  };

  nodes.protos = {
    host = "206.81.23.189";
    configuration = {
      imports = [
        ../config/machines/protos
      ];
      networking.public.ipv4 = "206.81.23.189";
      networking.public.ipv6 = "2a03:b0c0:3:d0::5f7f:5001";
      system.stateVersion = "19.03";
    };
  };

  nodes.vario = {
    host = "localhost";
    configuration = { lib, ... }: {
      imports = [
        ../config/machines/vario
        deployer
      ];
      system.stateVersion = "19.03";
    };
  };

  nodes.orakel = {
    host = "51.15.187.150";
    successTimeout = 120;
    configuration = {
      imports = [
        ../config/machines/orakel
      ];
      networking.public.ipv4 = "51.15.187.150";
      networking.public.ipv6 = "fe80::208:a2ff:fe0c:2ab4";
      system.stateVersion = "19.03";
    };
  };

}
