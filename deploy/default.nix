{ label, host }:

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

in
{
  network.description = "Infinisil's machines";

  defaults = { deploymentName, ... }: {
    system.nixos.label = label;
    imports = [ ../config ];

    nix.nixPath = [
      "nixos-config=${nixosConfig}"
    ];

    mine.deployer = {
      enableNixpkgs = true;
      remote = "git@github.com:Infinisil/system.git";
      nixops.state = ../external/private/deployments.nixops;
      nixops.deployment = deploymentName;
      directory = "/home/infinisil/cfg";
      nixpkgs = ../external/nixpkgs;
    };
  };

  protos = {
    deployment.targetHost = "104.248.129.84";
    imports = [
      ../config/machines/protos
      ../external/private/machines/protos.nix
    ];
  };

  ninur = { pkgs, ... }: {
    deployment.targetHost = if host == "emma"
      then "localhost" else "192.168.178.21";
    #deployment.targetHost = "10.149.76.3";
    deployment.hasFastConnection = true;
    imports = [
      ../config/machines/ninur
      ../external/private/machines/ninur.nix
    ];
    mine.deployer.enable = true;
  };

  vario = { pkgs, ... }: {
    deployment.targetHost = "localhost";
    #deployment.targetHost = "10.149.76.2";
    imports = [
      ../config/machines/vario
      ../external/private/machines/vario.nix
    ];
    mine.deployer.enable = true;
  };
}
