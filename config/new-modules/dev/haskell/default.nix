{ lib, config, pkgs, ... }:

with lib;

let

  hie = import (import ../../../sources).hie-nix {};

  hiebin = pkgs.writeScriptBin "hie" ''
    #!${pkgs.stdenv.shell}
    export LD_LIBRARY_PATH="${lib.makeLibraryPath [ pkgs.gmp ]}:$LD_LIBRARY_PATH"
    exec ${pkgs.direnv}/bin/direnv exec . ${hie.hies}/bin/hie-wrapper "$@"
  '';

  snack = (import (import ../../../sources).snack).snack-exe;

in

{

  options.mine.dev.haskell.enable = mkEnableOption "Haskell dev config";

  config = mkIf config.mine.dev.haskell.enable {

    environment.systemPackages = with pkgs; [
      hie.hies
      hiebin

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
