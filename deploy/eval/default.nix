let
  pkgs = import ../../external/nixpkgs {};
  nixops = pkgs.nixopsUnstable;

  infoFile = if builtins.pathExists ./info.json
    then builtins.readFile ./info.json
    else throw "info.json file not found, run ./genInfo to generate it";
  info = builtins.fromJSON infoFile;
  info' = pkgs.lib.mapAttrs (key: value:
    if key == "args" then
      pkgs.lib.mapAttrs (arg: expr: import (builtins.toFile arg expr)) value
      else value
  ) info;
  eval = import "${pkgs.nixopsUnstable}/share/nix/nixops/eval-machine-info.nix" info';
in eval
