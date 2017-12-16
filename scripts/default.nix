{ pkgs, lib, ... }:

lib.mapAttrs (file: type:
  pkgs.runCommand file {
    inherit (pkgs) acpi gawk bc;
  } ''
    substituteAll "${./.}/${file}" $out
    chmod +x $out
  ''
) (builtins.readDir ./.)
