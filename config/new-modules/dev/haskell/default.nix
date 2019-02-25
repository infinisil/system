{ lib, config, pkgs, ... }:

with lib;

let

  snack = (import (import ../../../sources).snack).snack-exe;

in

{

  options.mine.dev.haskell.enable = mkEnableOption "Haskell dev config";

  config = mkIf config.mine.dev.haskell.enable {

    environment.systemPackages = with pkgs; [
      stack

      cabal2nix

      haskellPackages.pointfree
      haskellPackages.stylish-haskell
      haskellPackages.hlint
      haskellPackages.structured-haskell-mode

      snack
    ];

    mine.emacs.config.haskell = true;

    mine.userConfig = {
      home.file.".ghci".text = ''
        :set prompt "\ESC[94m\STX \ESC[m\STX "
      '';
    };

  };
}
