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
    stdenv = basePkgs.stdenvNoCC;
    name = "nixpkgs-patched";
    src = sources.nixpkgs;
    patches = [
      ./config/patches/zfs-hibernate.patch
      # https://github.com/NixOS/nixpkgs/pull/479283
      #(basePkgs.fetchpatch {
      #  url = "https://github.com/NixOS/nixpkgs/pull/479283/commits/a3556b106651d6b246ba6b16823b24e16ad56739.patch";
      #  hash = "sha256-s22Tv4Ujanzz5sd2pTnKSB6hGJnoHBiW1Bag408+RPk=";
      #})
      #(basePkgs.fetchpatch {
      #  url = "https://github.com/NixOS/nixpkgs/pull/479283/commits/01a5efa47470cf5800f4e8b352d7bbe24b81e788.patch";
      #  hash = "sha256-3cOHpFOek6Wk0IN7tKC88Uuta1w8KSF+TqUrC5k8ndc=";
      #})
    ];
  };
in
import /home/infinisil/prj/nixus {
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

  defaults = { name, lib, options, ... }: {

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

      configuration = { lib, pkgs, options, ... }: {

        options._options = lib.mkOption {
          default = options;
        };

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
      system.stateVersion = "22.11";

      home-manager.sharedModules = [
        { home.stateVersion = "22.11"; }
      ];
    };
  };

  nodes.savior = {
    configuration = {
      imports = [
        (sources.home-manager + "/nixos")
        ./config/machines/savior
        deployer
      ];
    };
  };

  nodes.void = {
    configuration = {
      imports = [
        (sources.home-manager + "/nixos")
        ./config/machines/void
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
