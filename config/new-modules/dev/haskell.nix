{ lib, config, pkgs, ... }:

with lib;

let
  all-hies = import (import ../../sources).all-hies {};

  hie = all-hies.selection {
    selector = p: {
      inherit (p) ghc865 ghc883;
    };
  };
in
{

  options.mine.dev.haskell.enable = mkEnableOption "Haskell dev config";

  config = mkIf config.mine.dev.haskell.enable {

    environment.systemPackages = with pkgs; [
      #haskellPackages.pointfree
      #haskellPackages.stylish-haskell
      haskellPackages.hlint
      hie
    ];

    mine.userConfig = {
      home.file.".ghci".text = ''
        :set prompt "\ESC[94m\STX \ESC[m\STX "
      '';
    };

  };
}
