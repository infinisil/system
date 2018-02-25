{ pkgs, lib, config, ... }:

let

  nixpkgs-mozilla = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "42a0926f2f36cac2da53782259948ba071b6c6c5";
    sha256 = "1r2jglgl9k881byv1kc3rdda2lzaarvb0xn7nx3q0b3h25apjff5";
  };

in

with lib;

{

  options.mine.dev.rust.enable = mkEnableOption "Rust developer package";

  config = mkIf config.mine.dev.rust.enable {

    nixpkgs.overlays = [
      (import "${nixpkgs-mozilla}/rust-overlay.nix")
    ];

    environment.systemPackages = with pkgs.rustChannelOf {
      date = "2018-02-25";
      channel = "nightly";
    }; [
      rust
      (rust.override {
        extensions = [
          "rls-preview"
          "rustfmt-preview"
        ];
      })
    ];

  };

}
