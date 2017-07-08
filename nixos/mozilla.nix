{ config, pkgs, ... }:

# Custom packages
let
  mozillaPkgsDir = (import <nixpkgs>{config={};}).fetchFromGitHub {
    owner = "Infinisil";
    repo = "nixpkgs-mozilla";
    rev = "d13abfc71b2bbc6a6effda0b598c82b79c5e6512";
    sha256 = "0panppfq354qgdq1hf8dimdmmfz36s85nfn4c6ila75icrsd1mx8";
    fetchSubmodules = true;
  };

  mozillaPkgs = import mozillaPkgsDir {};

  rustOverlay = import "${mozillaPkgsDir}/rust-overlay.nix";
in
{
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [ rustOverlay ];

    environment.systemPackages = [
      mozillaPkgs.firefox-nightly-bin
    ];
}
