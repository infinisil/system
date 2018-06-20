{ pkgs, ... }:

{

  imports = [
    ./lidWakeDisable.nix
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

  hardware.bluetooth = {
    enable = true;
    powerOnBoot = false;
  };

  mine.hardware = {
    battery = true;
    cpuCount = 4;
    swap = true;
    touchpad = true;
    wlan = true;
    audio = true;
  };

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
}
