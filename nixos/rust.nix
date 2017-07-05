{ config, pkgs, ... }:

# Custom packages
let
  mozillaPkgsDir = (import <nixpkgs>{config={};}).fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "d3453b7b5fd3cb4b9b659ac4b0ffeb275413dcfe";
    sha256 = "0yv58nxffzwyjnhdmp8vbj4iqiab2q9m1pm21x1s4m10ama37nig";
    fetchSubmodules = true;
  };

  rustOverlay = import "${mozillaPkgsDir}/rust-overlay.nix";
in
{
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [ rustOverlay ];
}
