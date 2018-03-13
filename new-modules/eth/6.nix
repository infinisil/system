{ lib, pkgs, config, ... }:

with lib;

{

  options.mine.eth.sem6.enable = mkEnableOption "Stuff for the 6th semester";

  config = mkIf config.mine.eth.sem6.enable {
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql100;
    };

    mine.dev.java.enable = true;
  };

}
