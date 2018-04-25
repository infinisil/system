{ nodes, config, pkgs, lib, ... }:

with lib;

{
  imports = [
    ./hardware-configuration.nix
  ];

  mine.deluged.enable = true;

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

  services.znapzend = {
    enable = true;
    pure = true;
    zetup.main.plan = "1hour=>5min,1day=>1hour,1week=>1day,1month=>1week,1year=>1month,10years=>1year";
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
