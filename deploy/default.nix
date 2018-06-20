{ label }:

{
  network.description = "Infinisil's machines";

  defaults = {
    system.nixos.label = label;
    imports = [ ../config ];

    mine.deployer = {
      remote = "git@github.com:Infinisil/system.git";
      nixops.state = ../external/private/deployments.nixops;
      nixops.deployment = "infinisil";
      directory = "/home/infinisil/cfg";
      nixpkgs = ../external/nixpkgs;
    };
  };

  yuri = {
    deployment.targetHost = "10.149.76.1";
    imports = [ ../config/machines/yuri.nix ];
  };

  emma = { pkgs, ... }: {
    deployment.targetHost = "10.149.76.3";
    imports = [ ../config/machines/emma.nix ];
    mine.deployer.enable = true;
  };

  nepnep = { pkgs, ... }: {
    deployment.targetHost = "10.149.76.2";
    imports = [ ../config/machines/nepnep.nix ];
    mine.deployer.enable = true;
  };
}
