{ pkgs, config, lib, ... }:

let

  # TODO: Only use a single derivation
  packages = lib.mapAttrs (name: text:
    pkgs.writeShellScriptBin name (text + " \"$@\"")
  ) config.mine.binalias;
  result = pkgs.symlinkJoin {
    name = "binalias";
    paths = lib.attrValues packages;
  };

in

{
  options.mine.binalias = lib.mkOption {
    type = lib.types.attrsOf lib.types.str;
    default = {};
    description = ''
      Simple map from binary name to the command they should execute. Like bash
      aliases.
    '';
  };

  config = {
    environment.systemPackages = lib.mkIf (config.mine.binalias != {}) [ result ];
    environment.shellAliases = config.mine.binalias;
  };
}
