{ label }: {
  network.description = "Infinisil's machines";

  defaults = {
    system.nixos.label = label;
    imports = [ ../config ];
  };

  yuri = {
    deployment.targetHost = "10.149.76.1";
    imports = [ ../config/machines/yuri.nix ];
  };

  emma = { pkgs, ... }: {
    deployment.targetHost = "10.149.76.3";
    imports = [ ../config/machines/emma.nix ];
    environment.systemPackages = [
      (pkgs.callPackage ./rebuild.nix { })
    ];
  };

  nepnep = { pkgs, ... }: {
    deployment.targetHost = "10.149.76.2";
    imports = [ ../config/machines/nepnep.nix ];
    environment.systemPackages = [
      (pkgs.callPackage ./rebuild.nix { })
    ];
  };
}
