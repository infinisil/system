{ pkgs, lib, config, ... }:

with lib;

let

  cfg = config.mine.terminal;

in

{

  options.mine.terminal = {

    enable = mkEnableOption "My terminal";

  };

  config = mkIf cfg.enable (mkMerge [
    {

      environment.systemPackages = [
        pkgs.alacritty
      ];

    }
    (mkIf (config.mine.mainUser != null) {

      home-manager.users.${config.mine.mainUser} = {

        xdg.configFile."alacritty/alacritty.yml".source = ./alacritty.yml;

      };
    })
  ]);

}
