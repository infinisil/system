{ lib, pkgs, config, ... }: {
  options.mine.japaneseInput = lib.mkEnableOption "japanese input";

  config = lib.mkIf config.mine.japaneseInput {

    # Only really for env vars
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5.addons = [
        pkgs.fcitx5-mozc
      ];
    };

    # Would normally set this to fcitx, but kitty only supports ibus, and fcitx
    # provides an ibus interface. Can't use ibus for e.g. QT_IM_MODULE though,
    # because that at least breaks mumble
    environment.variables.GLFW_IM_MODULE = "ibus";

    mine.xUserConfig.xsession.initExtra = ''
      ${config.i18n.inputMethod.package}/bin/fcitx5 &
    '';

  };

}
