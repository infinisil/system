{
  lib, emacsWithPackages, runCommand, writeTextDir, makeWrapper,
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

  emacs = emacsWithPackages (epkgs: map (name: epkgs.${name}) epkgsNames);

  init = writeTextDir "init.el" (''
    (package-initialize)
  '' + concatMapStringsSep "\n\n" (filename: ''
    (load "${(if debug then toString else id) ./.}/${filename}")
  '') (builtins.attrNames elFilesWithPkgs));
in
  runCommand "custom-emacs" {
    buildInputs = [
      makeWrapper
    ];
    passthru = {
      inherit emacs init;
    };
  } ''
    mkdir $out

    # Link every folder in the original emacs
    for dir in ${emacs}/*; do
      ln -s $dir $out/$(basename $dir)
    done

    # Except bin
    rm $out/bin
    mkdir $out/bin

    # Because we link every file of bin
    for bin in ${emacs}/bin/*; do
      ln -s $bin $out/bin/$(basename $bin)
    done

    # Except emacs
    rm $out/bin/emacs

    # Because we wrap it with certain arguments
    makeWrapper ${emacs}/bin/emacs $out/bin/emacs \
      --add-flags -q \
      --add-flags "-l ${init}/init.el"
  ''
