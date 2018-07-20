{ lib, config, pkgs, ... }:
with lib;

{

  options.mine.firefox = mkOption {
    type = types.package;
    description = "Default firefox";
  };

  config = {
    environment.systemPackages = [ config.mine.firefox ];
    nixpkgs.config.firefox.enableBrowserpass = true;
    mine.firefox = pkgs.firefox-beta-bin;
  };

}
