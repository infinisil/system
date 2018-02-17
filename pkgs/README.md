# Packages

This folder contains a nixos module `default.nix` which will add an overlay.
The overlay adds every nix file in this folder as a package called with
`callPackage`, accessible through `pkgs.mine.<filename>`.

Example:

Adding a file named `test.nix` with the contents

```nix
{ pkgs }:

pkgs.writeText "test" "foobar"
```

makes `pkgs.mine.test` available.

