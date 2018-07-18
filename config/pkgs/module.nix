{ lib, ... }:

{

  nixpkgs.overlays = [
    (self: super: {
      mine = import ./default.nix {
        pkgs = self;
        inherit lib;
      };
    })
  ];

}
