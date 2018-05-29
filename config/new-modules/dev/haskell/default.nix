{ lib, config, pkgs, ... }:

with lib;

let

  hie-nix = pkgs.fetchFromGitHub {
    owner = "sectore";
    repo = "hie-nix";
    rev = "64f075b8d2f42b529b8cb1657c332c8b685479e8";
    sha256 = "1v25q340vzw5sjcn7b6kaswa896smb3g9mz0bz889n8krgxg4r0g";
  };

  stack2nixSrc = pkgs.fetchFromGitHub {
    owner = "sectore";
    repo = "stack2nix";
    rev = "4e4141d1f4a4626030f11bdf7623ccc7640f7b08";
    sha256 = "1v5pm770pmalxwvf6ddg196m17ga5lj30r1xq2sdd7fi330k0i27";
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
      haskellPackages.stylish-haskell
    ];

    mine.emacs.config.haskell = true;

    mine.userConfig = {
      home.file.".ghci".text = ''
        :set prompt "\ESC[94m\STX \ESC[m\STX "
      '';
    };

  };
}
