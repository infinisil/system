{ lib, sources, ... }:

{

  nixpkgs.overlays = [
    (self: super: {
      sources = sources;
      mine = import ./default.nix {
        pkgs = self;
        inherit lib;
      };
    })
  ];

}
