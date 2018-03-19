{ label }: {
  network.description = "Infinisil's machines";

  defaults = {
    deployment.alwaysActivate = true;
    system.nixos.label = label;

    imports = [
      ../private
      ../personal
      ../new-modules
      ../home-manager/nixos
      ../lib
      ../pkgs
    ];
  };

  yuri = {
    deployment.targetHost = "10.149.76.1";
    imports = [ ./yuri.nix ];
  };

  laptop = {
    deployment.targetHost = "10.149.76.3";
    imports = [ ./emma.nix ];
  };

  pc = {
    deployment.targetHost = "10.149.76.2";
    imports = [ ./nepnep.nix ];
  };
}
