{ }:

let
  pkgs = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/5a9c9162228bc0e3d13b316ecf1578bee2cd32e1.tar.gz";
    sha256 = "1gg0py29cimi80zl3ds0fpy8g5r8ygfjbd0sb7gng7d1551z1ybj";
  }) {};
in
  pkgs.alacritty
