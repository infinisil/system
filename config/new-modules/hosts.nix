{ config, lib, pkgs, ... }:

{
  mine.userConfig.programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "60";
    matchBlocks = lib.mapAttrs (name: value: { hostname = value; })
      config.networking.connections;
  };

}
