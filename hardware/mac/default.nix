{ pkgs, ... }:

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
}
