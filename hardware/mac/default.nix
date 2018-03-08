{ pkgs, lib, ... }:

{

  imports = [
    ./boot.nix
    ./hardware-configuration.nix
    ../../modules/zfs.nix
  ];

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  hardware.cpu.intel.updateMicrocode = true;

  # Seems to cause a bunch of problems
  # boot.kernelParams = [ "acpi_osi=" ];
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=2
  '';

  services.tlp.enable = true;

  mine.hasBattery = true;
  mine.wlan.enable = true;
  mine.touchpad.enable = true;

  services.xserver.videoDrivers = [ "intel" ];

  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
  ];

  environment.systemPackages = with pkgs; [
    efibootmgr
    acpi
    lm_sensors
  ];

  services.mbpfan.enable = true;

  services.znapzend = {
    pure = true;
    zetup."main/data" = rec {
      plan = "15min=>5min,1h=>15min,1d=>1h,1w=>1d,1m=>1w";
      recursive = true;
      destinations.backup = {
        dataset = "main/betty/backup";
        host = "192.168.1.25";
        plan = plan + ",1y=>1m";
      };
    };
  };

  home-manager.users.infinisil = {

    programs.htop.meters = {
      left = [
        "Memory"
        "CPU"
        "LeftCPUs2"
        "RightCPUs2"
        "Swap"
        { kind = "CPU"; mode = 3; }
      ];
      right = [
        { kind = "Clock"; mode = 4; }
        "Uptime"
        "Tasks"
        "LoadAverage"
        { kind = "Battery"; mode = 1; }
      ];
    };

  };
}
