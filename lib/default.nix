{ pkgs, lib, ... }:

{
  lib.mine.attrToDerivation = name: value: let
    # Maps an attrset/path to a build script that recursively creates the equivalent directory/file structure
    valToPath = name: value: if builtins.isAttrs value then ''
      mkdir "${name}" && pushd "${name}"
      ${lib.concatMapStringsSep "\n" (name: valToPath name value.${name}) (builtins.attrNames value)}
      popd
    '' else ''
      cp "${value}" "${name}"
    '';
    # Converts all (string) values of an attrset to a file that contains that string
    fileAttrSet = lib.mapAttrsRecursive (path: builtins.toFile (lib.concatStringsSep "-" path)) value;
  in pkgs.runCommand name {
    preferLocalBuild = true;
  } (valToPath "$out" fileAttrSet);
}
