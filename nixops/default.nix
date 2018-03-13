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
    deployment.targetHost = "207.154.198.134";
    imports = [ ./yuri.nix ];
  };

  laptop = {
    deployment.targetHost = "localhost";
    imports = [ ./emma.nix ];
  };

  pc = {
    deployment.targetHost = "pc";
    imports = [ ./nepnep.nix ];
  };
}
