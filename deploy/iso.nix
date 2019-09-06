(import ../external/nixpkgs/nixos {
  configuration = {

    imports = [ ../external/nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix ];

    i18n.consoleUseXkbConfig = true;
    services.xserver.xkbVariant = "dvp";

  };
}).config.system.build.isoImage
