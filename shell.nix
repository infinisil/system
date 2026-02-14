let
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs {
    config = {};
    overlays = [];
  };
in pkgs.mkShellNoCC {
  nativeBuildInputs = [
    pkgs.niv
    pkgs.jq
  ];
}
