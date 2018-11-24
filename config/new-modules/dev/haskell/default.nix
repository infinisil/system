{ lib, config, pkgs, ... }:

with lib;

let

  hie-nix = pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "a270d8db4551f988437ac5db779a3cf614c4af68";
    sha256 = "0hilxgmh5aaxg37cbdwixwnnripvjqxbvi8cjzqrk7rpfafv352q";
  };

  snackSrc = pkgs.fetchFromGitHub {
    owner = "nmattia";
    repo = "snack";
    rev = "fd3ddd7074d919980074eec39b19299e193e595c";
    sha256 = "08nasv39hnc7klagwgg2am3xgkinwh8rfaxy4amsi1cii29grxbp";
  };

  hie = import hie-nix {};

  hiebin = pkgs.writeScriptBin "hie" ''
    #!${pkgs.stdenv.shell}
    export LD_LIBRARY_PATH="${lib.makeLibraryPath [ pkgs.gmp ]}:$LD_LIBRARY_PATH"
    exec ${pkgs.direnv}/bin/direnv exec . ${hie.hies}/bin/hie-wrapper "$@"
  '';

  snack = (import snackSrc).snack-exe;

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
