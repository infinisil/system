{ config, lib, epkgs, dag, ... }:

with lib;

{

  options.idris = mkOption {
    type = types.bool;
    default = false;
    description = "idris config stuff";
  };

  config = mkIf config.idris {
    packages = with epkgs; [
      idris-mode
    ];
  };
}
