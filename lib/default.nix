{ pkgs, lib, ... }: with lib; with pkgs; {

  _module.args.mylib = {

    attrToDerivation = { name, value, strings ? true }: let
      # Maps an attrset/path to a build script that recursively creates the equivalent directory/file structure
      valToPath = name: value: if builtins.isAttrs value then ''
        mkdir ${name} && pushd ${name}
        ${concatMapStringsSep "\n" (name: valToPath (escapeShellArg name) value.${name}) (builtins.attrNames value)}
        popd
      '' else ''
        ln -s ${escapeShellArg (if strings then writeText "file" value else value)} ${name}
      '';
    in runCommand name {
      preferLocalBuild = true;
    } (valToPath "$out" value);
  };

}
