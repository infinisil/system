{ pkgs, lib, config, ... }:

with lib;

{

  options.mine.dev.rust.enable = mkEnableOption "Rust developer package";

  config = mkIf config.mine.dev.rust.enable {

    mine.emacs.config.rust = true;

    nixpkgs.overlays = [
      (import "${(import ../../sources).nixpkgs-mozilla}/rust-overlay.nix")
    ];

  };

}
