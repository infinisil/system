{ nodes, config, pkgs, lib, ... }:

with lib;

{
  imports = [
    ./hardware-configuration.nix
  ];

  mine.hardware = {
    swap = true;
    cpuCount = 8;
    audio = true;
  };

  mine.openvpn.client = {
    enable = true;
    server = nodes.yuri;
  };

  boot = {
    zfs.enableUnstable = true;
    loader = {
      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
      };
      efi.canTouchEfiVariables = true;
    };
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"
    KERNEL=="uinput", MODE="0660", GROUP="users", OPTIONS+="static_node=uinput"
  '';

  system.stateVersion = "18.03";

}
