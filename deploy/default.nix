{ label }:

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

  #yuri = {
  #  deployment.targetHost = "10.149.76.1";
  #  imports = [ ../config/machines/yuri ];
  #};

  new = {
    deployment.targetHost = "104.248.129.84";
    imports = [ ../config/machines/new ];
  };

  emma = { pkgs, ... }: {
    deployment.targetHost = "192.168.1.19";
    #deployment.targetHost = "10.149.76.3";
    deployment.hasFastConnection = true;
    imports = [ ../config/machines/emma ];
    mine.deployer.enable = true;
  };

  nepnep = { pkgs, ... }: {
    deployment.targetHost = "localhost";
    imports = [ ../config/machines/nepnep ];
    mine.deployer.enable = true;
  };
}
