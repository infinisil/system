{ pkgs }:

let

  nixpkgs = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "8b69791230943da18c7d00a10117a0f39da29ca0";
    sha256 = "024g9k3z6lji2xnfxnpil5p0h60qzxjf7rwd6ipnv76hky62dz8z";
  };

  old = import nixpkgs {};

in old.deluge
