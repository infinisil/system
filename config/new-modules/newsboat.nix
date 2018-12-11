{ pkgs, config, lib, ... }:

with lib;

let

  cfg = config.mine.newsboat;

in

{

  options.mine.newsboat = {
    enable = mkEnableOption "newsboat";

    config = mkOption {
      type = types.lines;
      default = "";
      description = "Config file for newsboat";
    };

    urls = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Feed urls";
    };
  };

  config = mkIf cfg.enable {

    environment.systemPackages = [ pkgs.newsboat ];

    mine.userConfig = {

      home.file.".newsboat/config".text = cfg.config;
      home.file.".newsboat/urls".text = lib.concatStringsSep "\n" cfg.urls;

    };

  };

}
