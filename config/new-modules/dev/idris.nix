{ config, lib, pkgs, ... }:

with lib;

let

  idris = with pkgs.idrisPackages; with-packages [
    base
    contrib
    effects
    pruviloj
    lightyear
    bi
  ];

in

{

  options.mine.dev.idris.enable = mkEnableOption "idris dev";

  config = mkIf config.mine.dev.idris.enable {

    environment.systemPackages = [ idris ];

    mine.emacs.config.idris = true;
  };

}
