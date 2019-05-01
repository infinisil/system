{ pkgs, config, lib, ... }:

with lib;

let

  # TODO: Only use a single derivation
  packages = mapAttrs (name: text:
    pkgs.writeShellScriptBin name (text + " \"$@\"")
  ) config.mine.binalias;
  result = pkgs.symlinkJoin {
    name = "binalias";
    paths = attrValues packages;
  };

in

{
  options.mine.binalias = mkOption {
    type = types.attrsOf types.str;
    default = {};
    description = ''
      Simple map from binary name to the command they should execute. Like bash
      aliases.
    '';
  };

  config.environment.systemPackages =
    mkIf (config.mine.binalias != {}) [ result ];

  # TODO: Add shell aliases too, then autocomplete can work

}
