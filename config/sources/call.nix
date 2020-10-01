{ file }: let
  fetchers = import ../../external/nixpkgs {
    overlays = [(self: super: {
      gitlabtoken = import ../../external/private/gitlabtoken;
      ethzgitlabtoken = import ../../external/private/ethzgitlabtoken;
    })];
  };
in fetchers.callPackage file {} // {
  inherit file;
}
