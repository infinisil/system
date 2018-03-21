{ pkgs, lib, ... }:

with lib;

let

  dir = ./.;

  # Recursively constructs an attrset of a given folder, recursing on directories, value of attrs is the filetype
  getDir = mapAttrs (file: type:
    if type == "directory" then getDir "${dir}/${file}" else type
  ) (builtins.readDir dir);

  # Tree of absolute paths of all files in the directory
  files = mapAttrsRecursive (path: type: "${dir}/${concatStringsSep "/" path}") getDir;

  # Map over each non-attributeset element including its name. The function f is called with arguments path, name and value. It needs to return a name, value pair.
  mapAttrsRecursive' = f: set:
  let
    recurse = path: set:
      let
        g =
          name: value:
          if isAttrs value then {
            inherit name;
            value = recurse (path ++ [name]) value;
          } else f path name value;
      in mapAttrs' g set;
  in recurse [] set;

  results = mapAttrsRecursive' (path: file: abs: {
    name = head (splitString "." file);
    value = if hasSuffix ".nix" file then
      pkgs.callPackage abs {} else
      abs;
  }) files;

in

{

  options.mine.assets = mkOption {
    type = types.attrs;
    default = {};
    description = ''
      All files in the assets folder, where each nix files will be transformed
      with callPackage.
    '';
  };

  config.mine.assets = results;

}
