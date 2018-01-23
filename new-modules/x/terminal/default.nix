{ pkgs, lib, config, ... }:

with lib;

let

  cfg = config.mine.terminal;

in

{

  options.mine.terminal = {

    enable = mkEnableOption "My terminal";

  };

  config = mkIf cfg.enable {

    environment.systemPackages = [
      pkgs.alacritty
    ];

    mine.userConfig = {

      xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yml;

    };

  };

}
