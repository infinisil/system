let
  pkgs = import ../external/nixpkgs {};
in

pkgs.stdenvNoCC.mkDerivation {
  name = "rb-env";
  nativeBuildInputs = with pkgs; [
    git
    jq
    vim
    nix
    nettools
  ];
}
