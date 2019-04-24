{ config, lib, ... }: {

  options.mine.saveSpace = lib.mkEnableOption "configure NixOS to save some space";

  config = lib.mkIf config.mine.saveSpace {

    nix = {
      autoOptimiseStore = true;
      gc = {
        automatic = true;
        dates = "daily";
        options = "--delete-older-than 7d";
      };
    };

    # Taken from <nixpkgs/nixos/modules/profiles/minimal.nix>
    i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];

    documentation.doc.enable = false;

    boot.cleanTmpDir = true;

  };
}
