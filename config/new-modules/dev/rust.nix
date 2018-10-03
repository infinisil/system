{ pkgs, lib, config, ... }:

let

  nixpkgs-mozilla = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "b9c99d043b1cb55ee8c08265223b7c35d687acb9";
    sha256 = "0akyhdv5p0qiiyp6940k9bvismjqm9f8xhs0gpznjl6509dwgfxl";
  };

  rust = let
    channel = pkgs.rustChannelOf {
      date = "2018-10-03";
      channel = "nightly";
    };
  in channel.rust.override {
    extensions = [
      "clippy-preview"
      "rls-preview"
      "rustfmt-preview"
      "rust-analysis"
      "rust-std"
      "rust-src"
    ];
  };

in

with lib;

{

  options.mine.dev.rust.enable = mkEnableOption "Rust developer package";

  config = mkIf config.mine.dev.rust.enable {

    nixpkgs.overlays = [
      (import "${nixpkgs-mozilla}/rust-overlay.nix")
    ];

    environment.systemPackages = [
      rust
    ];

  };

}
