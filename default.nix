let

  nixosConfig = builtins.toFile "configuration.nix" ''
    builtins.trace "This machine is managed by Nixus, using dummy configuration file for evaluation" {
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
      rb = "$HOME/cfg/rb";
    };
  };

  sources = import ./nix/sources.nix {};

in

{ nodes ? []
, deployHost ? null
, ignoreFailingSystemdUnits ? true
, deploySystem ? builtins.currentSystem
}:
let
  basePkgs = import sources.nixpkgs {
    overlays = [];
    config = {};
    system = deploySystem;
  };

  nixpkgs = basePkgs.srcOnly {
    name = "nixpkgs-patched";
    src = sources.nixpkgs;
    patches = [
      ./config/patches/zfs-hibernate.patch
    ];
  };
in
import sources.nixus {
  specialArgs.sources = sources;
  inherit deploySystem;
  nixpkgs = nixpkgs;
} {

  _file = ./default.nix;

  imports = [
    ./external/private
    ./config/multimods
  ];

  inherit deployHost;

  defaults = { name, lib, ... }: {

    options.configuration = lib.mkOption {
      type = lib.types.submoduleWith {
        modules = [];
        specialArgs.sources = sources;
      };
    };

    config = {
      enable = lib.elem name nodes;

      nixpkgs = nixpkgs;

      inherit ignoreFailingSystemdUnits;

      configuration = { lib, pkgs, ... }: {

        options.effectivePkgs = lib.mkOption {
          type = lib.types.raw;
          default = pkgs;
        };

        #config._module.args.sources = sources;

        config.nix.nixPath = [
          "nixos-config=${nixosConfig}"
          "nixpkgs=/etc/nixpkgs"
        ];

        config.environment.etc.nixpkgs.source = toString nixpkgs;
      };
    };
  };

  nodes.protos = {
    host = "root@206.81.23.189";
    switchTimeout = 240;
    configuration = {
      imports = [
        ./config
        ./external/private/default-old.nix
        (sources.home-manager + "/nixos")
        ./config/machines/protos
      ];
      networking.public.ipv4 = "206.81.23.189";
      networking.public.ipv6 = "2a03:b0c0:3:d0::5f7f:5001";
      system.stateVersion = "19.03";

      home-manager.sharedModules = [
        { home.stateVersion = "22.11"; }
      ];
    };
  };

  nodes.vario = {
    deployFrom.zion.host = "root@192.168.0.12";
    deployFrom.zion.hasFastConnection = true;
    switchTimeout = 240;
    configuration = {
      imports = [
        ./config
        ./external/private/default-old.nix
        (sources.home-manager + "/nixos")
        ./config/machines/vario
        deployer
      ];
      system.stateVersion = "22.11";

      home-manager.sharedModules = [
        { home.stateVersion = "22.11"; }
      ];
    };
  };

  nodes.zion = {
    deployFrom.vario.host = "root@192.168.0.17";
    deployFrom.vario.hasFastConnection = true;
    configuration = {
      imports = [
        (sources.home-manager + "/nixos")
        ./config/machines/zion
        deployer
      ];
      home-manager.sharedModules = [
        { home.stateVersion = "22.11"; }
      ];
    };
  };

  nodes.mac.enable = false;
  nodes.phone.enable = false;

}
