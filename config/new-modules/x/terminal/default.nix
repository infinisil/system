{ pkgs, lib, config, ... }:

with lib;

let

  cfg = config.mine.terminal;

in

{

  options.mine.terminal = {

    enable = mkEnableOption "My terminal";

    binary = mkOption {
      type = types.path;
      description = "Path to terminal binary";
    };

  };

  config = mkIf cfg.enable {

    mine.terminal.binary = "${pkgs.kitty}/bin/kitty";

    environment.systemPackages = [ pkgs.kitty ];

    mine.xUserConfig = {
      xdg.configFile."kitty/kitty.conf".source = ./kitty.conf;
    };

  };

}
