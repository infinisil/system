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

  emma = {
    deployment.targetHost = "10.149.76.3";
    imports = [ ../config/machines/emma.nix ];
  };

  nepnep = {
    deployment.targetHost = "10.149.76.2";
    imports = [ ../config/machines/nepnep.nix ];
  };
}
