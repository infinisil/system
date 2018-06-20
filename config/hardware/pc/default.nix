{ nodes, config, pkgs, lib, ... }:

with lib;

{
  mine.hardware = {
    swap = true;
    cpuCount = 8;
    audio = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  mine.compton.nvidia = true;
}
