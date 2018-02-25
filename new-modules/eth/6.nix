{ lib, pkgs, config, ... }:

with lib;

{

  options.mine.eth.sem6.enable = mkEnableOption "Stuff for the 6th semester";

  config.services.postgresql = mkIf config.mine.eth.sem6.enable {
    enable = true;
    package = pkgs.postgresql100;
  };

}
