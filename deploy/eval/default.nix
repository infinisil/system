let
  infoFile = if builtins.pathExists ./info.json
    then builtins.readFile ./info.json
    else throw "info.json file not found, run ./genInfo to generate it";
  info = builtins.fromJSON infoFile;
  eval = import ../../external/nixops/nix/eval-machine-info.nix info;
in eval
