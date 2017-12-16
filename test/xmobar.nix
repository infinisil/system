{ config, pkgs, lib, ... }:

with lib;
with types;

let
  cfg = config.services.xmobar;

  command = submodule {
    options = {

    };
  };
in

{
  options.config.services.xmobar = {
    enable = mkEnableOption "xmobar";

    fonts = mkOption {
      type = either str (listOf str);
      default = [];
      description = "Which fonts to use";
    };

    backgroundColor = mkOption {
      type = str;
    };

    foregroundColor = mkOption {
      type = str;
    };

    commands = mkOption {
      type = listOf command;
    };



  };

  config.services.xmobar = {
    backgroundColor = colors.hex 47 32 54;
    foregroundColor = colors.black;

    alpha = 210;

    template = [
      run
  };

}
