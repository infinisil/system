{ pkgs, lib, config, sources, ... }:

with lib;

let

  cfg = config.mine.terminal;

in

{

  options.mine.terminal = {

    enable = mkEnableOption "My terminal";

  };

  config = mkMerge [
    (mkIf cfg.enable {

      environment.systemPackages = [
        pkgs.kitty
      ];

      mine.xUserConfig = {
        xdg.configFile."kitty/kitty.conf".text = ''
          include ${sources.gruvbox-contrib}/kitty/gruvbox-light.conf
          include ${./kitty.conf}
        '';
      };

    })
    (mkIf config.mine.profiles.server.enable {
      environment.systemPackages = [
        (pkgs.kitty.overrideAttrs (old: {
          meta = old.meta // {
            outputsToInstall = [ "terminfo" ];
          };
        }))
      ];
    })
  ];

}
