{ file }: let
  pkgs = import ../../external/nixpkgs {
    overlays = [(self: super: {
      gitlabtoken = import ../../external/private/gitlabtoken;
    })];
  };
in pkgs.callPackage file {} // {
  inherit file;
}
