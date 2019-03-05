{ file }: let
  pkgs = import ../../external/nixpkgs {
    overlays = [(self: super: {
      gitlabtoken = import ../../external/private/gitlabtoken;
      ethzgitlabtoken = import ../../external/private/ethzgitlabtoken;
    })];
  };
in pkgs.callPackage file {} // {
  inherit file;
}
