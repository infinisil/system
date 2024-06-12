with import <nixpkgs> {};
mkShell {
  env.RUST_SRC_PATH = "${rustPlatform.rustLibSrc}";
  inputsFrom = [
    (callPackage ./package.nix { })
  ];
}
