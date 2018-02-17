{ lib, ... }:

{

  nixpkgs.overlays = [
    (self: super: {
      mine = import ./pkgs.nix {
        pkgs = self;
        inherit lib;
      };
    })
  ];

}
