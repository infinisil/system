{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.sound = {
    enable = mkEnableOption "sound config";
  };

  config = {

    mine.userConfig = {
      home.packages = [
        pkgs.mine.say
      ];
    };

  };

}
