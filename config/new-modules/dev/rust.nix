{ pkgs, lib, config, ... }:

let

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
      (import "${(import ../../sources).nixpkgs-mozilla}/rust-overlay.nix")
    ];

    environment.systemPackages = [
      rust
    ];

  };

}
