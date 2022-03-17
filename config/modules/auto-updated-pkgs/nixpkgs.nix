{ specFile }:
let
  spec = builtins.fromJSON (builtins.readFile specFile);
  nixpkgs = fetchTarball spec.url;
  pkgs = import nixpkgs {
    config = spec.config;
    overlays = spec.overlays;
  };
  package = pkgs.lib.getAttrFromPath spec.attrPath pkgs;
  final = package.override spec.overrides;
in final
