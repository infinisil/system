{ lib, config, pkgs, ... }:

with lib;

let

  hie-nix = pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "8f04568aa8c3215f543250eb7a1acfa0cf2d24ed";
    sha256 = "06ygnywfnp6da0mcy4hq0xcvaaap1w3di2midv1w9b9miam8hdrn";
  };

  stack2nixSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "stack2nix";
    rev = "9070f9173ae32f0be6f7830c41c8cfb8e780fdbf";
    sha256 = "1qz7yfd6icl5sddpsij6fqn2dmzxwawm7cb8aw4diqh71drr1p29";
  };

  hie = import hie-nix {};
  myStack2nix = import stack2nixSrc {};

in

{

  options.mine.dev.haskell.enable = mkEnableOption "Haskell dev config";

  config = mkIf config.mine.dev.haskell.enable {

    environment.systemPackages = with pkgs; [
      haskell.compiler.ghc822
      hie.hie82

      stack
      myStack2nix

      cabal2nix
      cabal-install

      haskellPackages.pointfree
      haskellPackages.stylish-haskell
      haskellPackages.hlint
    ];

    mine.emacs.config.haskell = true;

    mine.userConfig = {
      home.file.".ghci".text = ''
        :set prompt "\ESC[94m\STX \ESC[m\STX "
      '';
    };

  };
}
