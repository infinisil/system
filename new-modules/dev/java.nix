{ config, pkgs, lib, ... }:

with lib;
{

  options.mine.dev.java.enable = mkEnableOption "Java dev kit";

  config = mkIf config.mine.dev.java.enable {

    environment.systemPackages = [
      pkgs.mine.eclipse
    ];
  };

}
