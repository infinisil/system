{ lib, config, pkgs, ... }:

with lib;

let
  cfg = config.mine.keylayout;

  compiledLayout = pkgs.runCommand "layout.xkm" {
    nativeBuildInputs = [ pkgs.xorg.xkbcomp ];
  } "xkbcomp ${cfg.layoutFile} $out";

  loadLayout = pkgs.writeScriptBin "ll" ''
    #!${pkgs.stdenv.shell}
    ${pkgs.xorg.xkbcomp} ${compiledLayout} $DISPLAY
    ${pkgs.xcape}/bin/xcape -e '#94=Escape' &
  '';

in

{

  options.mine.keylayout = {
    enable = mkEnableOption "my keylayout config";

    layoutFile = mkOption {
      type = types.path;
      description = "Path to xkb layout file.";
    };
  };

  config = mkIf cfg.enable {

    # When in the DigitalOcean web console: `setxkbmap -layout us` (on local machine) and type in correct programmer dvorak
    i18n.consoleUseXkbConfig = true;

    environment.systemPackages = [ loadLayout ];

    services.xserver = {
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;

      # Basic keymap, is used for i18n virtual consoles
      layout = "us";
      xkbVariant = "dvp";
      xkbOptions = "caps:backspace";
      displayManager.sessionCommands = "${loadLayout}/bin/ll";
    };

  };
}
