{ lib, ... }:

with lib;

let

  mkDecl = attrs: mkOption ({
    readOnly = true;
    internal = true;
  } // attrs);

in

{
  options = {
    domain = mkDecl {
      type = types.str;
      description = "Domain of the server";
    };
  };

  config = {
    domain = "infinisil.com";
  };
}
