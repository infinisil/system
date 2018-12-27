let
  pkgs = import ../external/nixpkgs {};
in

pkgs.stdenv.mkDerivation {
  name = "rb-env";
  nativeBuildInputs = with pkgs; [
    nixopsUnstable
    git
    jq
    nix
  ];
}
