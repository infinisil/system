let
  pkgs = import ../../external/nixpkgs {};
  inherit (pkgs) lib;
  files = lib.filterAttrs (name: value:
    lib.hasSuffix ".nix" name
    && name != "default.nix"
  ) (builtins.readDir ./.);
in
  lib.mapAttrs' (name: value: let
    file = toString ./. + "/${name}";
  in {
    name = lib.removeSuffix ".nix" name;
    value = import file { inherit pkgs; } // {
      inherit file;
    };
  }) files
