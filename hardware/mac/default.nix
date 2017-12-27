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

  hardware.cpu.intel.updateMicrocode = true;

  environment.systemPackages = with pkgs; [
    efibootmgr
    acpi
    lm_sensors
  ];

  services.mbpfan.enable = true;

  powerManagement.cpuFreqGovernor = lib.mkForce "powersave";

  services.znapzend = {
    pure = true;
    zetup."main/data" = rec {
      plan = "15min=>5min,1h=>15min,1d=>1h,1w=>1d,1m=>1w";
      recursive = true;
      destinations.backup = {
        dataset = "betty/backup";
        plan = plan + ",1y=>1m";
        presend = "zpool import -f betty | true";
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