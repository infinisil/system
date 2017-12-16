{
  pkgs ? import <nixpkgs> {},
  config ? builtins.readFile ./xmonad.hs,
  extraPackages ? self: with self; [
    xmonad-contrib
    fuzzy
  ]
}:
pkgs.stdenv.mkDerivation {
  name = "xmonad-compiled";
  src = pkgs.writeTextDir "xmonad.hs" config;

  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (self: [ self.xmonad ] ++ extraPackages self))
  ];

  buildPhase = ''
    ghc --make xmonad.hs
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv xmonad $out/bin
  '';
}
