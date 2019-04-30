{ config, lib, ... }: {

  options.mine.saveSpace = lib.mkEnableOption "configure NixOS to save some space";

  config = lib.mkIf config.mine.saveSpace {

    nix = {
      autoOptimiseStore = true;
      # I can't have this because of https://github.com/NixOS/nix/issues/719 :/
      #gc = {
      #  automatic = true;
      #  # TODO: Increase interval to e.g. every 10 minutes?
      #  dates = "daily";
      #  # TODO: Add -v so I can see progress when it deletes links
      #  options = "--delete-older-than 7d";
      #};
    };

    # Taken from <nixpkgs/nixos/modules/profiles/minimal.nix>
    i18n.supportedLocales = [ (config.i18n.defaultLocale + "/UTF-8") ];

    documentation.doc.enable = false;

    boot.cleanTmpDir = true;

  };
}
