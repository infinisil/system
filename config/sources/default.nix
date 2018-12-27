let
  lib = import ../../external/nixpkgs/lib;
  files = lib.filterAttrs (name: value:
    lib.hasSuffix ".nix" name
    && name != "default.nix"
    && name != "call.nix"
  ) (builtins.readDir ./.);
in
  lib.mapAttrs' (name: value: let
    file = toString ./. + "/${name}";
  in {
    name = lib.removeSuffix ".nix" name;
    value = import ./call.nix {
      inherit file;
    };
  }) files
