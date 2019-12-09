{ config, pkgs, lib, ... }: {

  imports = [
    ./boot.nix
    ./hardware-configuration.nix
    ./lidWakeDisable.nix
  ];

  mine.enableUser = true;

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';
  mine.compton.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  # Seems to cause a bunch of problems
  # boot.kernelParams = [ "acpi_osi=" ];
  boot.extraModprobeConfig = ''
    options hid_apple fnmode=1
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
    #steam
    efibootmgr
    acpi
    lm_sensors
    slack
  ];

  services.mbpfan.enable = true;

  services.openvpn.servers.orakel = {
    mine.type = "client";
    mine.client.serverIp = config.networking.connections.orakel;
  };

  services.znapzend = {
    enable = true;
    autoCreation = true;
    pure = true;
    zetup."tank/root/data" = rec {
      plan = "1h=>5min,1d=>1h,1w=>1d";
      recursive = true;
      destinations.backup = {
        host = config.networking.connectivitySpec.local.ninur.vario.ethernet;
        dataset = "main/backup/ninur";
        plan = "1d=>1h,1m=>1d,1y=>1m";
      };
    };
  };

  mine.string-transfer.enable = true;

  mine.profiles.desktop.enable = true;

  networking = {
    hostName = "ninur";
    hostId = "34cc680d";
  };
}
