{ lib, rustPlatform }:
let
  fs = lib.fileset;
in
rustPlatform.buildRustPackage {
  name = "obswatch";
  src = fs.toSource {
    root = ./.;
    fileset = fs.unions [
      ./Cargo.lock
      ./Cargo.toml
      ./src
    ];
  };
  cargoLock.lockFile = ./Cargo.lock;
}
