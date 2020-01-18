{ lib, ... }:
let inherit (lib) types;
in {

  options.private.passwords = lib.mkOption {
    type = types.attrsOf types.str;
    description = "Passwords";
  };

}
