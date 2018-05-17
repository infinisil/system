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
      pkgs.tmux
    ];

    mine.xUserConfig = {

      xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yml;

      home.file.".tmux.conf".text = ''
        set -g default-terminal "xterm-256color"
        set -g destroy-unattached "on"
      '';

    };

  };

}