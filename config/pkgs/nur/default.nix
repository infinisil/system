{ pkgs, lib }:
let
  blacklist = [
    # Require IFD:
    "hashsearch"
    "deluge"
    "nix"

    # Requires passwords:
    "say"
  ];
in

lib.filterAttrs (name: val: ! lib.elem name blacklist) (
  import ../default.nix {
    inherit pkgs lib;
    passwords = false;
  }
)
