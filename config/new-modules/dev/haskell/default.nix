{ lib, config, pkgs, ... }:

with lib;

let

  hie-nix = pkgs.fetchFromGitHub {
    owner = "infinisil";
    repo = "hie-nix";
    rev = "c02228671ee1dcb652afba91e56272b11000eb1e";
    sha256 = "10g9xnzq2mzqsxybyz80pf1mhxr1mn1kj8vqbdf08c8nglcjpzvp";
  };

  stack2nixSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "stack2nix";
    rev = "9070f9173ae32f0be6f7830c41c8cfb8e780fdbf";
    sha256 = "1qz7yfd6icl5sddpsij6fqn2dmzxwawm7cb8aw4diqh71drr1p29";
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
    exec ${pkgs.direnv}/bin/direnv exec . ${hie.hies}/bin/hie-wrapper "$@"
  '';

  myStack2nix = import stack2nixSrc {};
  snack = (import snackSrc).snack-exe;

in

{

  options.mine.dev.haskell.enable = mkEnableOption "Haskell dev config";

  config = mkIf config.mine.dev.haskell.enable {

    environment.systemPackages = with pkgs; [
      hie.hies
      hiebin

      stack
      myStack2nix

      cabal2nix
      cabal-install

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
