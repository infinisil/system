{
  stdenv, lib, emacsPackagesNg, fetchFromGitHub, writeTextDir, writeScriptBin, makeWrapper,
  debug ? false
}:

with lib;

let

  elFiles = filterAttrs (name: type:
    (type == "regular" && hasSuffix ".el" name)
  ) (builtins.readDir ./.);

  # Mapping from filename -> list of epkgs packages to include
  elFilesWithPkgs = mapAttrs (name: type:
    let firstLine = head (splitString "\n" (builtins.readFile "${./.}/${name}")); in
    tail (splitString " " firstLine)
  ) elFiles;

  epkgsNames = flatten (builtins.attrValues elFilesWithPkgs);

  overrides = super: self: {
  };

  emacsWithPackages' = (emacsPackagesNg.overrideScope overrides).emacsWithPackages;

  emacs = emacsWithPackages' (epkgs: map (name: epkgs.melpaPackages.${name} or epkgs.${name}) epkgsNames);

  init = writeTextDir "init.el" (''
    (package-initialize)
  '' + concatMapStringsSep "\n\n" (filename: ''
    (load "${(if debug then toString else id) ./.}/${filename}")
  '') (builtins.attrNames elFilesWithPkgs));
in
  lib.extendDerivation true {
    inherit emacs init;
  } (writeScriptBin "emacs" ''
    #!${stdenv.shell}
    ${emacs}/bin/emacs -q -l ${init}/init.el
  '')
