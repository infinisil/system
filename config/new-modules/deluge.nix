{ config, lib, pkgs, ... }:

with lib;

{

  options.mine.deluged.enable = mkEnableOption "deluge daemon config";

  config = mkIf config.mine.deluged.enable {

    nixpkgs.overlays = [(self: super: {
      deluge = pkgs.mine.deluge;
    })];

    services.deluge.enable = true;

    networking.firewall.allowedTCPPorts = [ 58846 ];


    users.users = mkMerge (map (user: {
      ${user}.extraGroups = [ "deluge" ];
    }) config.mine.mainUsers);

  };

}
