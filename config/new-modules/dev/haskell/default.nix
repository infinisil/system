{ lib, config, pkgs, ... }:

with lib;

let

  hie-nix = pkgs.fetchFromGitHub {
    owner = "infinisil";
    repo = "hie-nix";
    rev = "6e41b5fa155e30be4801af7fbc674633ca6c4d0c";
    sha256 = "1m1hglas0ql41hbg80fn1fzl2277ad0qf2plim54vqga7lis7555";
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
  myStack2nix = import stack2nixSrc {};
  snack = (import snackSrc).snack-exe;

in

{

  options.mine.dev.haskell.enable = mkEnableOption "Haskell dev config";

  config = mkIf config.mine.dev.haskell.enable {

    environment.systemPackages = with pkgs; [
      hie.hies

      stack
      myStack2nix

      cabal2nix
      cabal-install

      haskellPackages.pointfree
      haskellPackages.stylish-haskell
      haskellPackages.hlint

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
