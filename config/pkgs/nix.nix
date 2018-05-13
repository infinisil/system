{ path }:
let
  nixSrc = fetchGit {
    url = https://github.com/NixOS/nix;
    rev = "1ad19232c4bbecb06e6acbb2a3a538544a28e1f2";
  };
  nixRelease = import "${nixSrc}/release.nix" {
    nix = nixSrc;
    nixpkgs = path;
  };
in nixRelease.build.x86_64-linux
