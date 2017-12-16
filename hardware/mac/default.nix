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
