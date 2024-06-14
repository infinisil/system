let
  sources = import ./nix/sources.nix {};

  configuration = {
    imports = [
      (sources.nixpkgs + "/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
    ];

    programs.neovim = {
      enable = true;
      vimAlias = true;
      defaultEditor = true;
    };

    i18n.consoleUseXkbConfig = true;
    services.xserver.xkbVariant = "dvp";
  };

  nixos = import (sources.nixpkgs + "/nixos") {
    inherit configuration;
  };
in nixos.config.system.build.isoImage
