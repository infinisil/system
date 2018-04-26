{ lib, config, pkgs, ... }:

with lib;

let

  hie-nix = pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "dbb89939da8997cc6d863705387ce7783d8b6958";
    sha256 = "1bcw59zwf788wg686p3qmcq03fr7bvgbcaa83vq8gvg231bgid4m";
  };

  stack2nixSrc = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "stack2nix";
    rev = "b614c56ea3c36d9f5940fb1852b7957cad586547";
    sha256 = "1nk3pax43zz15lqn2j963zkn9x9na7q2kid9vk5kxif9daymq4wd";
  };

  hie = import hie-nix {};

  myStack2nix = import "${stack2nixSrc}" {};

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
    ];

    mine.emacs.config.haskell = true;

  };
}
