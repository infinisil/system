{ pkgs, lib, ... }:

{

  imports = [
    ./boot.nix
    ./samba.nix
    ./hardware-configuration.nix
    ../../modules/wlan.nix
    ../../modules/touchpad.nix
    ../../modules/zfs.nix
  ];

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  hardware.cpu.intel.updateMicrocode = true;

  boot.kernelParams = [ "acpi_osi=" ];

  services.tlp.enable = true;

  mine.hasBattery = true;

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
